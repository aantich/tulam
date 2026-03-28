{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- | Pre-CLM monomorphization: resolve implicit-param dispatch at the Surface AST level.
--
-- This pass runs BEFORE CLM conversion (between Pass 3.1 and Pass 4).
-- At the Surface level, we have full type information: Lambda params carry
-- concrete types (filled in by the type annotation pass), the type checker
-- has run, and we can directly substitute target instance bodies.
--
-- After this pass, no implicit-param function calls remain in the AST for
-- functions that have concrete instance implementations. exprToCLM then
-- generates only CLMAPP (direct calls), never CLMIAP (dispatch).
--
-- Design principle: monomorphize where types are available (Surface AST),
-- not where they've been erased (CLM). The type checker does ALL inference;
-- this pass only does instance resolution.
module Monomorphize
    ( monomorphizeForTarget
    , monomorphizeLambdas
    , monomorphizePass
    , resolveImplicitCalls
    , inferExprTypeName
    , buildHandlerOpsMap
    , lowerStageRExpr
    ) where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Control.Applicative ((<|>))
import Data.List (foldl', partition, sortOn)
import Control.Monad.Trans.State.Strict (get, modify)

import Surface
import Logs (SourceInfo(..))
import State
import qualified ElabMetadata as EM
import ElabMetadata (binderRole, binderVisibility, defaultExplicitBinderInfo, isSemanticRole)
import ElabCore
import ElabObligation
import qualified Debug.Trace as DT

-- | Type environment: maps variable names to their concrete type names.
-- Built from Lambda params (which now have concrete types after the TC annotation pass).
type TypeEnv = HashMap Name Name

-- | Conditional debug trace: emits a message when debug flag is True.
-- Uses Debug.Trace.trace (unsafePerformIO) so it works in pure code.
dbg :: Bool -> String -> a -> a
dbg False _ x = x
dbg True msg x = DT.trace ("[mono] " ++ msg) x

-- ============================================================================
-- Pipeline Integration (Pass 3.5)
-- ============================================================================

-- | Optional monomorphization pass for the compilation pipeline.
-- Controlled by the `monomorphLevel` compiler flag.
-- When MonoNone (default for interpreter), this is a no-op.
-- When MonoFull, resolves all instance dispatch to direct calls.
monomorphizePass :: IntState ()
monomorphizePass = do
    st <- get
    let flags = currentFlags st
        level = monomorphLevel flags
    case level of
        MonoNone -> return ()  -- no-op for interpreter
        _ -> do
            let env = currentEnvironment st
                debug = debugTrace flags
                -- For pipeline-level mono, we don't have a specific target.
                -- Use empty target name: only resolves instanceLambdas (MonoFull),
                -- not target-specific externs. Backend CompileDriver adds those.
                env' = monomorphizeForTarget debug level "" env
            modify (\s -> s { currentEnvironment = env' })
            verboseLog "  Pass 3.5: monomorphization complete"

-- ============================================================================
-- Core API
-- ============================================================================

-- | Monomorphize all top-level and instance lambdas for a given target.
-- Replaces implicit-param calls (like (+)(x,y)) with target instance bodies
-- (like __add_i64(x,y)) wherever concrete types can be determined.
-- Also resolves effect handler operations (putStrLn, readRef, etc.) to their
-- target handler bodies.
monomorphizeForTarget :: Bool -> MonomorphLevel -> Name -> Environment -> Environment
monomorphizeForTarget debug monoLevel targetName env =
    let tInstances = fromMaybe Map.empty (Map.lookup targetName (targetInstances env))
        handlerOps = buildHandlerOpsMap targetName env
        -- Use elaborated lambdas (with Typed wrappers) as input when available
        topInput  = topLambdasByRep ElabLambdaRep env
        instInput = instanceLambdasByRep ElabLambdaRep env
        topLams' = Map.map (resolveInLambda debug monoLevel env tInstances handlerOps) topInput
        instLams' = Map.map (resolveInLambda debug monoLevel env tInstances handlerOps) instInput
        -- Write monomorphized results to elab maps, preserving raw canonical maps
    in env { topLambdasElab = topLams', instanceLambdasElab = instLams' }

-- | Monomorphize specific sets of lambdas, using the full environment for lookups.
-- Used by CompileDriver for reachability-driven compilation.
monomorphizeLambdas :: Bool -> MonomorphLevel -> Name -> Environment
                    -> HashMap Name Lambda -> HashMap Name Lambda
                    -> (HashMap Name Lambda, HashMap Name Lambda)
monomorphizeLambdas debug monoLevel targetName env topLams instLams =
    let tInstances = fromMaybe Map.empty (Map.lookup targetName (targetInstances env))
        handlerOps = buildHandlerOpsMap targetName env
    in ( Map.map (resolveInLambda debug monoLevel env tInstances handlerOps) topLams
       , Map.map (resolveInLambda debug monoLevel env tInstances handlerOps) instLams
       )

-- ============================================================================
-- Instance Resolution (the core of monomorphization)
-- ============================================================================

-- | Build a map from effect operation name → handler Lambda body.
buildHandlerOpsMap :: Name -> Environment -> HashMap Name Lambda
buildHandlerOpsMap targetName env =
    case Map.lookup targetName (targetHandlers env) of
        Nothing -> Map.empty
        Just handlers ->
            Map.fromList
                [ (lamName lam, lam)
                | (_handlerName, (_effectName, _params, impls)) <- Map.toList handlers
                , Function lam <- impls
                ]

-- | Resolve implicit-param calls within a single Lambda.
-- Reads types from params (annotated by TC) and the Lambda's return type.
resolveInLambda :: Bool -> MonomorphLevel -> Environment -> NameMap Lambda -> HashMap Name Lambda -> Lambda -> Lambda
resolveInLambda debug monoLevel env tInstances handlerOps lam =
    let typeEnv = buildTypeEnv (params lam)
        mRetType = exprToTypeName (lamType lam)
        -- Phase 2.3: Check both topLambdasR and instanceLambdasR for StageR representation.
        -- This ensures obligation info from TC is used for instance lambdas too.
        mRLam = case Map.lookup (lamName lam) (topLambdasR env) of
            Just rlam -> Just rlam
            Nothing   -> Map.lookup (lamName lam) (instanceLambdasR env)
        body' = case mRLam of
            Just rlam -> lowerStageRExpr debug monoLevel env tInstances handlerOps typeEnv mRetType (elabLamBody rlam)
            Nothing -> resolveImplicitCalls debug monoLevel env tInstances handlerOps typeEnv mRetType (body lam)
    in lam { body = body' }

-- | Build a type environment from function parameters.
-- After the TC annotation pass, most params have concrete types.
buildTypeEnv :: [Var] -> TypeEnv
buildTypeEnv = Map.fromList . mapMaybe extractVarType
  where
    extractVarType (Var _ (Implicit _) _) = Nothing  -- type param, not a value
    extractVarType (Var n t _) = case exprToTypeName t of
        Just tn -> Just (n, tn)
        Nothing -> Nothing

-- | Extract a concrete type name from a Surface type expression.
-- Phase 5.1: Extended to handle more forms — Pi types, universes, and
-- resolved metas (Id "?tN" from Phase 1.2 resolution are not concrete).
exprToTypeName :: Expr -> Maybe Name
exprToTypeName (Typed _ t) = exprToTypeName t
exprToTypeName (Id n)
    | not (null n) && head n >= 'A' && head n <= 'Z' = Just n
    | n == "Int" || n == "Float64" || n == "String" || n == "Char" || n == "Bool"
      || n == "Unit" || n == "Nat" || n == "Byte" || n == "Array" = Just n
exprToTypeName (App f _)
    | Just n <- calleeHeadName f
    , not (null n)
    , head n >= 'A' && head n <= 'Z' = Just n
-- Pi types: extract the return type name (for morphism dispatch)
exprToTypeName (Pi _ _ ret) = exprToTypeName ret
-- Concrete primitive types at universe level
exprToTypeName (U _) = Nothing  -- universes are not dispatchable
-- Unsolved meta — not concrete
exprToTypeName (Meta _) = Nothing
exprToTypeName _ = Nothing

calleeHeadName :: Expr -> Maybe Name
calleeHeadName (Id n) = Just n
calleeHeadName (Typed e _) = calleeHeadName e
calleeHeadName _ = Nothing

lowerStageRExpr :: Bool -> MonomorphLevel -> Environment -> NameMap Lambda -> HashMap Name Lambda -> TypeEnv -> Maybe Name -> ElabExpr -> Expr
lowerStageRExpr debug monoLevel env tInstances handlerOps typeEnv mLamRet = lower
  where
    lower (EVar n) = Id n
    lower (EGlobal n) = Id n
    lower (ELit l) = Lit l
    lower (EType e) = e
    lower (EAnn e ty) = Typed (lower e) ty
    lower (ELam binders bodyExpr retTy) =
        let vars = [Var (elabBinderName b) (elabBinderType b) UNDEFINED | b <- binders]
            innerTypeEnv = Map.union (Map.fromList [(name v, tn) | v <- vars, Just tn <- [exprToTypeName (typ v)]]) typeEnv
            loweredBody = lowerStageRExpr debug monoLevel env tInstances handlerOps innerTypeEnv (exprToTypeName retTy <|> mLamRet) bodyExpr
        in Function (mkLambda "<stage-r-lam>" vars loweredBody retTy)
    lower (ELet binds bodyExpr) =
        let binds' = [(Var n UNDEFINED UNDEFINED, lower e) | (n, e) <- binds]
            inferredEnv = buildTypeEnvFromLetPairs env typeEnv binds'
            letTypeEnv = Map.union inferredEnv typeEnv
        in LetIn binds' (lowerStageRExpr debug monoLevel env tInstances handlerOps letTypeEnv mLamRet bodyExpr)
    lower (EProject fa base retTy) =
        let projected = RecFieldAccess fa (lower base)
        in case retTy of
            UNDEFINED -> projected
            _ -> Typed projected retTy
    lower (EIf c t f retTy) =
        let e = IfThenElse (lower c) (lower t) (lower f)
        in case retTy of
            UNDEFINED -> e
            _ -> Typed e retTy
    lower (ECase alts retTy) =
        case alts of
            [ElabAlt checks bodyExpr] ->
                let e = ExpandedCase (Prelude.map lower checks) (lower bodyExpr) SourceInteractive
                in case retTy of
                    UNDEFINED -> e
                    _ -> Typed e retTy
            _ -> case retTy of
                    UNDEFINED -> UNDEFINED
                    _ -> Typed UNDEFINED retTy
    lower (ESurface e) = resolveImplicitCalls debug monoLevel env tInstances handlerOps typeEnv mLamRet e
    lower (ECall kind headExpr args retTy) =
        let -- Lower arguments with callee parameter type context for morphism dispatch.
            -- For DirectCall to a known function, propagate param types as mLamRet context
            -- so that nested SemanticCalls (e.g., convert(True) inside nat_to_int) can
            -- resolve morphism dispatch using the expected return type from the call site.
            loweredArgs = case (kind, calleeHeadName (lower headExpr)) of
                (DirectCall, Just funcName) ->
                    case lookupLambdaRep ElabLambdaRep funcName env of
                        Just calleeLam ->
                            let valParams = filter (not . isImplicitVar) (params calleeLam)
                            in [ case (safeIdx i valParams >>= exprToTypeName . typ) of
                                     Just ptn | not (null ptn) && head ptn >= 'A' && head ptn <= 'Z' ->
                                         lowerStageRExpr debug monoLevel env tInstances handlerOps typeEnv (Just ptn) (elabArgExpr a)
                                     _ -> lower (elabArgExpr a)
                               | (i, a) <- zip [0..] args
                               ]
                        Nothing -> map (lower . elabArgExpr) args
                _ -> map (lower . elabArgExpr) args
            safeIdx i xs = if i < length xs then Just (xs !! i) else Nothing
            loweredHead = lower headExpr
            wrapRet e = case exprToTypeName retTy of
              Just _ -> Typed e retTy
              Nothing -> e
            -- | Check if a type name is concrete (starts with uppercase), not a type variable.
            isConcreteTypeName n = not (null n) && head n >= 'A' && head n <= 'Z'
            -- | Return type name from ECall retTy (propagated from Typed wrapper).
            callRetTypeName = exprToTypeName retTy
            resolveByObligation funcName obl =
              case obl of
                HandlerObligation{} -> case Map.lookup funcName handlerOps of
                  Just handlerLam -> goSurface (substituteInstanceBody handlerLam loweredArgs)
                  Nothing -> wrapRet (App (Id funcName) loweredArgs)
                StructureObligation{} ->
                  let inferredArgs = catMaybes (map (inferExprTypeName env typeEnv) loweredArgs)
                      inferredOblArgs = catMaybes (map (inferExprTypeName env typeEnv) (oblExprArgs obl))
                      -- Only use oblTypeArgs if they contain concrete types (not type variables like "a", "b")
                      concreteOblTypes = filter isConcreteTypeName (oblTypeArgs obl)
                      types1 = if not (null concreteOblTypes) then concreteOblTypes
                               else if not (null inferredArgs) then inferredArgs
                               else inferredOblArgs
                      -- Morphism fallback: append return type from ECall retTy for multi-param dispatch
                      typesWithRet = case callRetTypeName of
                          Just r | r `notElem` types1 -> types1 ++ [r]
                          _ -> types1
                      -- Try with return type first (morphisms), then without
                      tryResolve ts = case resolveTargetInstanceMulti tInstances funcName ts of
                          Just resolvedBody -> Just $ wrapRet (goSurface (substituteInstanceBody resolvedBody loweredArgs))
                          Nothing -> case monoLevel of
                            MonoFull -> case resolveInstanceLambdaWithKey env funcName ts of
                              Just (instKey, _) -> Just $ dbg debug ("  [stageR] " ++ funcName ++ " → instance " ++ instKey ++ " for " ++ show ts) $
                                                   wrapRet (App (Id instKey) loweredArgs)
                              Nothing -> Nothing
                            _ -> Nothing
                  in dbg debug ("[stageR] resolveByObl " ++ funcName ++ " oblTypes=" ++ show (oblTypeArgs obl)
                                ++ " concrete=" ++ show concreteOblTypes ++ " inferred=" ++ show inferredArgs
                                ++ " retTy=" ++ show callRetTypeName ++ " types1=" ++ show types1
                                ++ " withRet=" ++ show typesWithRet) $
                     case tryResolve typesWithRet of
                      Just result -> result
                      Nothing -> case tryResolve types1 of
                          Just result -> result
                          Nothing -> dbg debug ("  [stageR] UNRESOLVED " ++ funcName ++ " types=" ++ show typesWithRet) $
                                     wrapRet (App (Id funcName) loweredArgs)
        in case (kind, calleeHeadName loweredHead) of
            (DirectCall, _) -> wrapRet (App loweredHead loweredArgs)
            (IntrinsicCall _, _) -> wrapRet (App loweredHead loweredArgs)
            (EvidenceCall wit, _) ->
              case wit of
                InstanceWitness{witnessKey=instKey} -> wrapRet (App (Id instKey) loweredArgs)
                HandlerWitness{} ->
                  case Map.lookup (witnessMethod wit) handlerOps of
                    Just handlerLam -> wrapRet (substituteInstanceBody handlerLam loweredArgs)
                    Nothing -> wrapRet (App loweredHead loweredArgs)
            (SemanticCall obl, Just funcName) -> resolveByObligation funcName obl
            (HandlerCall obl, Just funcName) -> resolveByObligation funcName obl
            _ -> wrapRet (App loweredHead loweredArgs)

    goSurface = resolveImplicitCalls debug monoLevel env tInstances handlerOps typeEnv mLamRet

-- | Walk a Surface expression, resolving implicit-param function calls
-- to their target instance bodies where possible.
--
-- The Lambda's return type (mLamRet) is used for morphism dispatch where
-- the return type is needed (e.g., Convertible(Nat, Int)).
-- Unlike the previous implementation, we do NOT propagate expected types
-- through sub-expressions — the TC has already done that work.
resolveImplicitCalls :: Bool -> MonomorphLevel -> Environment -> NameMap Lambda
                     -> HashMap Name Lambda -> TypeEnv -> Maybe Name -> Expr -> Expr
resolveImplicitCalls debug monoLevel env tInstances handlerOps typeEnv mLamRet = go
  where
    go expr = case expr of
        -- The key case: function application where funcName has implicit params.
        -- Be elaboration-aware: the callee head may be wrapped in Typed.
        App f args
            | Just funcName <- calleeHeadName f ->
                let -- Check if function has implicit params that need dispatch.
                    -- Two ways a function can need dispatch:
                    -- 1. Semantic role (evidence/handler): classic algebra methods like (+), show
                    -- 2. Has ANY implicit params AND instance lambdas exist: morphism methods
                    --    like convert, where implicit params are typed as Type but dispatch is needed
                    isSemanticImplicit = case lookupLambdaRep ElabLambdaRep funcName env of
                        Just fun ->
                            let infos = lookupAnyLambdaBinderInfo funcName env
                                hasSemanticBinder = or
                                    [ binderVisibility bi == EM.Implicit && isSemanticRole (binderRole bi)
                                    | bi <- take (length (params fun)) (infos ++ repeat defaultExplicitBinderInfo)
                                    ]
                                -- Also check: has implicit params AND registered instance lambdas
                                hasImplicitWithInstances = hasImplicit fun
                                    && hasAnyInstanceLambda funcName env
                            in hasSemanticBinder || hasImplicitWithInstances
                        _ -> False
                    hasTargetInstance = hasAnyTargetInstance funcName tInstances
                    -- Phase 1: Don't pre-process lambda args with `go` — instead pass
                    -- them to resolveImplicitCall which enriches them with callee type info
                    -- before recursing into their bodies.
                    smartArgs = map (\a -> case unwrapTyped a of
                        Function lam | any (\v -> not (isImplicitVar v) && typ v == UNDEFINED) (params lam) -> a  -- keep unenriched
                        _ -> go a) args
                    unwrapTyped (Typed e _) = e
                    unwrapTyped e = e
                    -- Check if there are untyped lambda args that need enrichment
                    hasUntypedLamArgs = any (\a -> case unwrapTyped a of
                        Function lam -> any (\v -> not (isImplicitVar v) && typ v == UNDEFINED) (params lam)
                        _ -> False) args
                in if isSemanticImplicit || hasTargetInstance || Map.member funcName handlerOps
                    then resolveImplicitCall funcName smartArgs
                    -- Even for non-implicit functions: enrich lambda args using callee signature
                    -- so that implicit calls INSIDE the lambda body can resolve.
                    -- E.g., flip(fn(x,y) = x - y, 10, 42) — enrich x,y to Int from flip's sig.
                    else if hasUntypedLamArgs
                        then let argTypes = map (inferExprTypeName env typeEnv) args
                                 knownTypes = catMaybes argTypes
                                 enrichedArgs = enrichLambdaArgsFromCallee funcName knownTypes args
                             in App (go f) (map go enrichedArgs)
                        else App (go f) (map go args)

        -- Nullary implicit-param reference (e.g., `zero`, `one`, `empty`, `readLine`)
        Id funcName ->
            case lookupLambdaRep ElabLambdaRep funcName env of
                Just fun
                    | let infos = lookupAnyLambdaBinderInfo funcName env
                          semanticImplicit = or
                            [ binderVisibility bi == EM.Implicit && isSemanticRole (binderRole bi)
                            | bi <- take (length (params fun)) (infos ++ repeat defaultExplicitBinderInfo)
                            ]
                    , semanticImplicit ->
                        case Map.lookup funcName handlerOps of
                            Just handlerLam -> substituteInstanceBody handlerLam []
                            Nothing -> expr
                _ -> expr

        -- Desugared let-in: App (Function λ(x). body) [val]
        -- Extend typeEnv with param types inferred from actual args.
        App (Function lam) args ->
            let args' = map go args
                paramTypeEnv = buildTypeEnv (params lam)
                inferredParamTypes = Map.fromList
                    [ (name v, tn)
                    | (v, arg) <- zip (params lam) args'
                    , Nothing <- [exprToTypeName (typ v)]
                    , Just tn <- [inferExprTypeName env typeEnv arg]
                    ]
                innerTypeEnv = Map.unions [inferredParamTypes, paramTypeEnv, typeEnv]
                body' = resolveImplicitCalls debug monoLevel env tInstances handlerOps innerTypeEnv mLamRet (body lam)
            in App (Function (lam { body = body' })) args'

        -- Recurse into all expression forms
        App f args -> App (go f) (map go args)
        -- Phase 1.3: For standalone lambdas, use param types (which may have been
        -- enriched by callee-directed propagation from processArgsWithCalleeContext)
        Function lam ->
            let paramEnv = buildTypeEnv (params lam)
                innerTypeEnv = Map.union paramEnv typeEnv
                innerRet = case exprToTypeName (lamType lam) of
                    Just r  -> Just r
                    Nothing -> mLamRet
            in Function $ lam { body = resolveImplicitCalls debug monoLevel env tInstances handlerOps innerTypeEnv innerRet (body lam) }
        LetIn binds bodyExpr ->
            let binds' = map (\(v, ex) -> (v, go ex)) binds
                inferredEnv = buildTypeEnvFromLetPairs env typeEnv binds'
                letTypeEnv = Map.union inferredEnv typeEnv
            in LetIn binds' (resolveImplicitCalls debug monoLevel env tInstances handlerOps letTypeEnv mLamRet bodyExpr)
        -- Typed (App f args) t: propagate t as return type context for dispatch.
        -- This enables morphism dispatch (e.g., convert(x) : Int) and return-type-driven
        -- dispatch (e.g., toEnum(65) : Char) in the Surface path.
        Typed e@(App _ _) t ->
            let retName = exprToTypeName t
                innerRet = retName <|> mLamRet
            in Typed (resolveImplicitCalls debug monoLevel env tInstances handlerOps typeEnv innerRet e) t
        Typed e t -> Typed (go e) t
        ExpandedCase checks bodyExpr si ->
            ExpandedCase (map goPatternCheck checks) (go bodyExpr) si
        PatternMatches cases -> PatternMatches (map go cases)
        CaseOf cv ex si ->
            let patTypes = extractPatternTypes env typeEnv cv
                innerTypeEnv = Map.union patTypes typeEnv
                goInner = resolveImplicitCalls debug monoLevel env tInstances handlerOps innerTypeEnv mLamRet
            in CaseOf (map (\v -> v { val = goInner (val v) }) cv) (goInner ex) si
        ConTuple ct args -> ConTuple ct (map go args)
        NTuple fields -> NTuple (map (\(mn, e) -> (mn, go e)) fields)
        DeclBlock exprs -> DeclBlock (map go exprs)
        ActionBlock stmts -> ActionBlock (map goActionStmt stmts)
        IfThenElse c t e -> IfThenElse (go c) (go t) (go e)
        BinaryOp op l r -> BinaryOp op (go l) (go r)
        RecFieldAccess ac e -> RecFieldAccess ac (go e)
        ReprCast e t -> ReprCast (go e) t
        ArrayLit es -> ArrayLit (map go es)
        Instance sn mTag ta impls reqs -> Instance sn mTag ta (map go impls) reqs
        _ -> expr  -- Lit, Id (non-implicit), U, UNDEFINED, Intrinsic, etc.

    -- | Resolve a single implicit-param function call.
    -- Tries: handler ops → target instances → instanceLambdas (MonoFull).
    -- Phase 1: After inferring arg types, enriches lambda closure args with
    -- type info from the callee's signature before final resolution.
    resolveImplicitCall :: Name -> [Expr] -> Expr
    resolveImplicitCall funcName args' =
        -- Try handler operation first (putStrLn, readRef, etc.)
        case Map.lookup funcName handlerOps of
            Just handlerLam ->
                dbg debug ("resolve " ++ funcName ++ " → handler op") $
                go (substituteInstanceBody handlerLam args')
            Nothing ->
                let argTypes = map (inferExprTypeName env typeEnv) args'
                    knownTypes = catMaybes argTypes
                    -- Phase 1: Enrich lambda args using callee-directed type propagation
                    enrichedArgs = enrichLambdaArgsFromCallee funcName knownTypes args'
                    enrichedArgTypes = map (inferExprTypeName env typeEnv) enrichedArgs
                in case sequence enrichedArgTypes of
                    Just types ->
                        dbg debug ("resolve " ++ funcName ++ " types=" ++ show types) $
                        resolveWithTypes funcName types enrichedArgs
                    Nothing ->
                        -- Partial: try with just the known types
                        let enrichedKnown = catMaybes enrichedArgTypes
                        in if null enrichedKnown
                            then dbg debug ("UNRESOLVED " ++ funcName ++ " — no arg types inferred, args=" ++ show (map showExprBrief enrichedArgs)) $
                                 App (Id funcName) enrichedArgs
                            else dbg debug ("resolve " ++ funcName ++ " partial types=" ++ show enrichedKnown) $
                                 resolveWithTypes funcName enrichedKnown enrichedArgs

    -- | Phase 1: Enrich lambda closure arguments using the callee's type signature.
    -- When we know some arg types (e.g., Int from literal 0, Array from [1,2,3]),
    -- use the callee's param types to infer what the lambda params should be.
    enrichLambdaArgsFromCallee :: Name -> [Name] -> [Expr] -> [Expr]
    enrichLambdaArgsFromCallee funcName knownArgTypes args'
        | null knownArgTypes = args'  -- nothing to propagate
        | not (any isUntypedFunctionArg args') = args'  -- no lambda args to enrich
        | otherwise =
            case lookupLambdaRep ElabLambdaRep funcName env of
                Nothing -> args'
                Just calleeLam ->
                    let implParams = filter isImplicitVar (params calleeLam)
                        rawValParams = filter (not . isImplicitVar) (params calleeLam)
                        -- For structure methods, params only has implicit type params.
                        -- Extract value param types from the function type signature instead.
                        valParams = if null rawValParams
                            then extractVirtualValParams calleeLam
                            else rawValParams
                        -- Collect all type var names (explicit + free in param types)
                        allTypeVarNames = nub' (map name implParams ++ concatMap (collectFreeTypeVars . typ) valParams)
                        -- Build type var bindings from known arg types
                        tvBindings0 = inferTypeVarBindings implParams valParams args' knownArgTypes
                        -- Enhance with element type inference from arrays
                        tvBindings = inferTypeVarBindingsWithElements tvBindings0 valParams args'
                    in if Map.null tvBindings then args'
                       else dbg debug ("  callee-directed " ++ funcName ++ ": tvBindings=" ++ show (Map.toList tvBindings)) $
                            zipWith (enrichArg valParams tvBindings) [0..] args'
      where
        isUntypedFunctionArg (Function lam) =
            any (\v -> not (isImplicitVar v) && typ v == UNDEFINED) (params lam)
        isUntypedFunctionArg (Typed (Function lam) _) =
            any (\v -> not (isImplicitVar v) && typ v == UNDEFINED) (params lam)
        isUntypedFunctionArg _ = False

        -- For structure methods where params only has implicit type params,
        -- extract value params from the inner lambda body (the implementation).
        extractVirtualValParams :: Lambda -> [Var]
        extractVirtualValParams lam = case lamType lam of
            Function innerLam -> filter (not . isImplicitVar) (params innerLam)
            piType@(Pi _ _ _) -> synthVarsFromPiChain piType
            _ -> case body lam of
                Function innerLam -> filter (not . isImplicitVar) (params innerLam)
                _ -> []

        synthVarsFromPiChain :: Expr -> [Var]
        synthVarsFromPiChain (Pi Nothing dom rest) =
            Var ("__p" ++ show (length (synthVarsFromPiChain rest) + 1)) dom UNDEFINED : synthVarsFromPiChain rest
        synthVarsFromPiChain (Pi (Just n) dom rest)
            | isUniverseType dom = synthVarsFromPiChain rest
            | otherwise = Var n dom UNDEFINED : synthVarsFromPiChain rest
        synthVarsFromPiChain _ = []

        -- Build type variable bindings by matching known arg types against callee param types.
        -- Collects ALL free lowercase type variables from callee param types,
        -- not just implicit param names (e.g., for foldl: a, b from `f: b -> a -> b`).
        inferTypeVarBindings :: [Var] -> [Var] -> [Expr] -> [Name] -> HashMap Name Name
        inferTypeVarBindings implParams valParams argExprs _knownTypes =
            let -- Collect all type variable names: explicit implicit params + free lowercase vars
                explicitTVs = map name implParams
                freeTypeTVs = concatMap (collectFreeTypeVars . typ) valParams
                typeVarNames = nub' (explicitTVs ++ freeTypeTVs)
                -- For each arg, try to match its type against the corresponding callee param type
                bindings = catMaybes
                    [ case (inferExprTypeName env typeEnv argExpr, safeIndex i valParams) of
                        (Just tn, Just calleeParam) ->
                            matchParamTypeVars typeVarNames (typ calleeParam) tn
                        _ -> Nothing
                    | (i, argExpr) <- zip [0..] argExprs
                    , not (isFunctionExpr argExpr)  -- skip lambda args for inference
                    ]
            in Map.fromList (concat bindings)

        -- Collect free lowercase identifiers from a type expression (type variables)
        collectFreeTypeVars :: Expr -> [Name]
        collectFreeTypeVars (Id n) | not (null n) && head n >= 'a' && head n <= 'z' = [n]
        collectFreeTypeVars (App f as) = concatMap collectFreeTypeVars (f : as)
        collectFreeTypeVars (Pi _ a b) = collectFreeTypeVars a ++ collectFreeTypeVars b
        collectFreeTypeVars _ = []

        isFunctionExpr (Function _) = True
        isFunctionExpr _ = False

        safeIndex :: Int -> [a] -> Maybe a
        safeIndex i xs | i < length xs = Just (xs !! i)
                       | otherwise = Nothing

        -- Match a param type against a concrete type to extract type var bindings.
        -- Also infers element types from array arguments when possible.
        matchParamTypeVars :: [Name] -> Expr -> Name -> Maybe [(Name, Name)]
        matchParamTypeVars tvNames (Id n) concrete
            | n `elem` tvNames = Just [(n, concrete)]
        matchParamTypeVars tvNames (App (Id n) typeArgs) concrete
            | n `elem` tvNames = Just [(n, concrete)]
            | otherwise =
                -- c(a) matching against "Array" — c = Array, but need to infer a separately
                -- Try to get a from the actual argument expression's element type
                Just $ catMaybes
                    [ if tn `elem` tvNames
                      then Just (n, concrete)  -- bind outer: c = Array
                      else Nothing
                    | Id tn <- [Id n]  -- the constructor type var (c)
                    ] ++
                    -- For inner type vars (like a in c(a)), try to infer from array elements
                    -- This will be filled by inferElementTypes below
                    []
        matchParamTypeVars _ _ _ = Nothing

        -- After initial type var bindings, try to infer remaining vars from array element types
        inferTypeVarBindingsWithElements :: HashMap Name Name -> [Var] -> [Expr] -> HashMap Name Name
        inferTypeVarBindingsWithElements initial valParams argExprs =
            let missing = [tv | tv <- concatMap (collectFreeTypeVars . typ) valParams
                              , not (Map.member tv initial)]
            in if null missing then initial
               else foldl' (\acc (i, argExpr) ->
                   let unwrapped = case argExpr of { Typed e _ -> e; e -> e }
                   in case (safeIndex i valParams, unwrapped) of
                       (Just param, ArrayLit (e:_)) ->
                           -- For Array args, infer element type from first element
                           case inferExprTypeName env typeEnv e of
                               Just elemType ->
                                   let paramTy = typ param
                                       -- If param type is c(a), and we know c = Array,
                                       -- then the element is a
                                       innerBindings = matchElementTypeVars (map name (filter isImplicitVar (params calleeLam)) ++ missing) paramTy elemType
                                   in Map.union innerBindings acc
                               Nothing -> acc
                       _ -> acc
                   ) initial (zip [0..] argExprs)
              where
                calleeLam = case lookupLambdaRep ElabLambdaRep funcName env of
                    Just l -> l
                    Nothing -> error "unreachable: calleeLam"

                -- Match element type vars: if param type is c(a) and elem type is "Int", bind a = Int
                matchElementTypeVars :: [Name] -> Expr -> Name -> HashMap Name Name
                matchElementTypeVars tvNames (App _ innerArgs) elemType =
                    Map.fromList [(n, elemType) | Id n <- innerArgs, n `elem` tvNames]
                matchElementTypeVars _ _ _ = Map.empty

        -- Enrich a single argument if it's an untyped lambda
        enrichArg :: [Var] -> HashMap Name Name -> Int -> Expr -> Expr
        -- Handle Typed-wrapped functions
        enrichArg valParams tvBindings i (Typed (Function lam) t) =
            case enrichArg valParams tvBindings i (Function lam) of
                Function lam' -> Typed (Function lam') t
                other -> other
        enrichArg valParams tvBindings i (Function lam)
            | i < length valParams
            , any (\v -> not (isImplicitVar v) && typ v == UNDEFINED) (params lam) =
                let calleeParamType = typ (valParams !! i)
                    expectedTypes = extractArrowTypes tvBindings calleeParamType
                    enrichedParams = zipEnrichParams (params lam) expectedTypes
                    enrichedTypeEnv = Map.union
                        (Map.fromList [(name v, tn) | v <- enrichedParams
                                      , not (isImplicitVar v)
                                      , Just tn <- [exprToTypeName (typ v)]])
                        typeEnv
                    innerRet = case exprToTypeName (lamType lam) of
                        Just r  -> Just r
                        Nothing -> mLamRet
                    body' = resolveImplicitCalls debug monoLevel env tInstances handlerOps enrichedTypeEnv innerRet (body lam)
                in dbg debug ("  enriched lambda params: " ++ show [(name v, typ v) | v <- enrichedParams, not (isImplicitVar v)]) $
                   Function (lam { params = enrichedParams, body = body' })
        enrichArg _ _ _ arg = arg

        -- Extract parameter types from an arrow type, applying type var substitutions
        extractArrowTypes :: HashMap Name Name -> Expr -> [Expr]
        extractArrowTypes tvBinds (Pi Nothing dom cod) =
            applyTVSubst tvBinds dom : extractArrowTypes tvBinds cod
        extractArrowTypes tvBinds (Pi (Just n) dom cod)
            | isUniverseType dom = extractArrowTypes tvBinds cod  -- skip type params
            | otherwise = applyTVSubst tvBinds dom : extractArrowTypes tvBinds cod
        extractArrowTypes _ _ = []

        applyTVSubst :: HashMap Name Name -> Expr -> Expr
        applyTVSubst tvBinds (Id n) = case Map.lookup n tvBinds of
            Just concrete -> Id concrete
            Nothing -> Id n
        applyTVSubst tvBinds (App f as) = App (applyTVSubst tvBinds f) (map (applyTVSubst tvBinds) as)
        applyTVSubst _ e = e

        isUniverseType (U _) = True
        isUniverseType (Id "Type") = True
        isUniverseType (Id "Type1") = True
        isUniverseType _ = False

        -- Zip params with inferred types, only enriching UNDEFINED ones
        zipEnrichParams :: [Var] -> [Expr] -> [Var]
        zipEnrichParams [] _ = []
        zipEnrichParams (v:vs) ts
            | isImplicitVar v = v : zipEnrichParams vs ts
            | otherwise = case ts of
                (t:ts') | typ v == UNDEFINED -> v { typ = t } : zipEnrichParams vs ts'
                (_:ts') -> v : zipEnrichParams vs ts'
                [] -> v : vs

    -- | Try to resolve given known arg types, with return-type-aware dispatch.
    resolveWithTypes :: Name -> [Name] -> [Expr] -> Expr
    resolveWithTypes funcName types args' =
        let -- Exact key lookup only (no single-param fallback) for multi-param keys
            tryExact ts = Map.lookup (mkInstanceKey funcName ts Nothing) tInstances
            -- Full cascade including single-param fallback
            tryTarget ts = resolveTargetInstanceMulti tInstances funcName ts
            applyTarget ts resolvedBody =
                dbg debug ("  " ++ funcName ++ " → target instance for " ++ show ts) $
                let result = go (substituteInstanceBody resolvedBody args')
                in wrapWithReturnType funcName types result
        in case mLamRet of
            Just ret | ret `notElem` types ->
                -- Strategy: morphism exact key first, then arg dispatch (with return-type validation),
                -- then return-type only, then fallback.
                case tryExact (types ++ [ret]) of
                    Just rb -> applyTarget (types ++ [ret]) rb
                    _ -> case tryTarget types of
                        Just rb | instanceRetMatchesFn funcName ret ->
                            applyTarget types rb
                        _ -> case tryTarget [ret] of
                            Just rb -> applyTarget [ret] rb
                            _ -> case tryTarget types of
                                Just rb -> applyTarget types rb
                                Nothing -> tryInstanceLambdaFallback funcName types args'
            _ ->
                case tryTarget types of
                    Just rb -> applyTarget types rb
                    Nothing -> tryInstanceLambdaFallback funcName types args'

    -- | Check if arg-type dispatch is valid for a function, or if it has
    -- return-type-only type variables that require return-type dispatch.
    -- Returns True if arg-type dispatch is fine (e.g., == dispatches on arg types).
    -- Returns False if the function has type vars only in the return type (e.g., toEnum).
    instanceRetMatchesFn :: Name -> Name -> Bool
    instanceRetMatchesFn funcName' _expectedRet =
        not (dispatchesOnReturnType funcName')

    -- | Cached check: does this function dispatch on its return type?
    -- True for functions like toEnum where the implicit type param only appears
    -- in the return type, not in value parameter types.
    dispatchesOnReturnType :: Name -> Bool
    dispatchesOnReturnType fn =
        case lookupLambdaRep ElabLambdaRep fn env <|> lookupLambda fn env of
            Just lam ->
                let implParams = filter isImplicitVar (params lam)
                    valParams = filter (not . isImplicitVar) (params lam)
                    valParamTypeVars = concatMap (collectLowercaseIds . typ) valParams
                    retTypeVars = collectLowercaseIds (lamType lam)
                    retOnlyVars = filter (\tv -> tv `elem` map name implParams && tv `notElem` valParamTypeVars) retTypeVars
                    result = not (null retOnlyVars)
                in result
            Nothing -> False
      where
        collectLowercaseIds (Id n) | not (null n) && head n >= 'a' && head n <= 'z' = [n]
        collectLowercaseIds (App f as) = concatMap collectLowercaseIds (f : as)
        collectLowercaseIds (Pi _ a b) = collectLowercaseIds a ++ collectLowercaseIds b
        collectLowercaseIds (Function lam) = collectLowercaseIds (lamType lam)
        collectLowercaseIds _ = []

    -- | Wrap a resolved expression with the function's return type if determinable.
    -- After target instance substitution, the result may not carry type info.
    -- We reconstruct it from the algebra function's return type + concrete arg types.
    wrapWithReturnType :: Name -> [Name] -> Expr -> Expr
    wrapWithReturnType _ _ e@(Typed _ _) = e  -- already typed
    wrapWithReturnType funcName argTypes result =
        case lookupLambda funcName env of
            Just lam | lamType lam /= UNDEFINED ->
                -- Resolve type variables in return type using arg types
                let implParams = filter isImplicitVar (params lam)
                    valParams = filter (not . isImplicitVar) (params lam)
                    -- Build type subst: match param types against concrete arg types
                    typeVarNames = map name implParams
                    typeSubst = Map.fromList $ zip typeVarNames (map Id argTypes)
                    resolvedRetType = substRetType typeSubst (lamType lam)
                in case exprToTypeName resolvedRetType of
                    Just _ -> Typed result resolvedRetType
                    Nothing -> result
            _ -> result

    substRetType :: HashMap Name Expr -> Expr -> Expr
    substRetType subst (Id n) = fromMaybe (Id n) (Map.lookup n subst)
    substRetType subst (App f args) = App (substRetType subst f) (map (substRetType subst) args)
    substRetType _ e = e

    -- | Try resolving via instanceLambdas when MonoFull.
    -- Intrinsic instances ARE resolved to their instance key — CLM/bytecode handle
    -- intrinsic dispatch via CLMPRIMCALL. The monomorphizer's job is just to resolve
    -- WHICH instance to call, not whether the body is native code or interpreted.
    tryInstanceLambdaFallback :: Name -> [Name] -> [Expr] -> Expr
    tryInstanceLambdaFallback funcName argTypes args'
        | monoLevel /= MonoFull = App (Id funcName) args'
        | otherwise =
            -- Try arg+ret first (morphisms), then args alone, then ret-only (return-type dispatch).
            let withRet = case mLamRet of
                    Just ret | ret `notElem` argTypes -> resolveInstanceLambdaWithKey env funcName (argTypes ++ [ret])
                    _ -> Nothing
                withRetOnly = case mLamRet of
                    Just ret | ret `notElem` argTypes -> resolveInstanceLambdaWithKey env funcName [ret]
                    _ -> Nothing
            in case withRet of
                Just (instKey, instLam)
                    | body instLam /= Intrinsic ->
                        dbg debug ("  " ++ funcName ++ " → instance " ++ instKey ++ " (morphism dispatch)") $
                        App (Id instKey) args'
                    | otherwise ->
                        dbg debug ("  " ++ funcName ++ " → intrinsic " ++ instKey ++ " (morphism, kept as dispatch)") $
                        App (Id funcName) args'
                _ ->
                    case resolveInstanceLambdaWithKey env funcName argTypes of
                      Just (instKey, instLam)
                        | body instLam /= Intrinsic ->
                            dbg debug ("  " ++ funcName ++ " → instance " ++ instKey) $
                            App (Id instKey) args'
                        | otherwise ->
                            dbg debug ("  " ++ funcName ++ " → intrinsic " ++ instKey ++ " (kept as dispatch)") $
                            App (Id funcName) args'
                      Nothing -> case withRetOnly of
                        Just (instKey, instLam)
                            | body instLam /= Intrinsic ->
                                dbg debug ("  " ++ funcName ++ " → instance " ++ instKey ++ " (ret-type dispatch)") $
                                App (Id instKey) args'
                            | otherwise ->
                                dbg debug ("  " ++ funcName ++ " → intrinsic " ++ instKey ++ " (ret-type, kept as dispatch)") $
                                App (Id funcName) args'
                        _ ->
                            dbg debug ("  UNRESOLVED " ++ funcName ++ " — no instance found for types=" ++ show argTypes) $
                            App (Id funcName) args'

    goPatternCheck (PatternGuard pc e) = PatternGuard pc (go e)
    goPatternCheck other = other

    goActionStmt (ActionExpr e) = ActionExpr (go e)
    goActionStmt (ActionBind n e) = ActionBind n (go e)
    goActionStmt (ActionLet n e) = ActionLet n (go e)

-- ============================================================================
-- Instance Resolution Helpers
-- ============================================================================

-- | Resolve an instance lambda using a cascade mirroring the runtime dispatcher.
resolveInstanceLambdaWithKey :: Environment -> Name -> [Name] -> Maybe (Name, Lambda)
resolveInstanceLambdaWithKey env funcName types =
    let instLams = instanceLambdas env
        key1 = mkInstanceKey funcName types Nothing
        exact = Map.lookup key1 instLams
    in firstJust'
    -- 1. Exact multi-param: "convert\0Nat\0Int"
    [ fmap (key1,) exact
    -- 2. Any-tag: "combine\0Nat\0@Additive"
    , let baseKey = mkInstanceKey funcName types Nothing
          tagPrefix = baseKey ++ "\0@"
          matches = sortOn fst $ Map.toList $ Map.filterWithKey
                      (\k _ -> take (length tagPrefix) k == tagPrefix) instLams
      in case matches of
          ((k, v):_) -> Just (k, v)
          [] -> Nothing
    -- 3. Prefix (morphisms): "convert\0Nat" matches "convert\0Nat\0Int"
    , let key = mkInstanceKey funcName types Nothing
          prefix = key ++ "\0"
      in case Map.lookup key instLams of
          Just lam -> Just (key, lam)
          Nothing ->
              let matches = Map.toList $ Map.filterWithKey
                      (\k _ -> take (length prefix) k == prefix) instLams
                  (direct, composed) = partition (isDirectInstanceLam funcName . snd) matches
              in case direct of
                  ((k, v):_) -> Just (k, v)
                  [] -> case composed of
                      ((k, v):_) -> Just (k, v)
                      [] -> Nothing
    -- 4. Deduped types
    , let uniq = nub' types
      in if uniq /= types
         then let key = mkInstanceKey funcName uniq Nothing
              in fmap (key,) $ Map.lookup key instLams
         else Nothing
    -- 5. Single-param for each type
    , firstJust [ let key = mkInstanceKey funcName [t] Nothing
                  in fmap (key,) $ Map.lookup key instLams
                | t <- nub' types ]
    -- 6. Single-param any-tag
    , firstJust [ let tagPrefix = mkInstanceKey funcName [t] Nothing ++ "\0@"
                      matches = sortOn fst $ Map.toList $ Map.filterWithKey
                                  (\k _ -> take (length tagPrefix) k == tagPrefix) instLams
                  in case matches of
                      ((k, v):_) -> Just (k, v)
                      [] -> Nothing
                | t <- nub' types ]
    ]

firstJust' :: [Maybe a] -> Maybe a
firstJust' [] = Nothing
firstJust' (Just x : _) = Just x
firstJust' (Nothing : rest) = firstJust' rest

-- | Build type env from let bindings.
buildTypeEnvFromLetPairs :: Environment -> TypeEnv -> [(Var, Expr)] -> TypeEnv
buildTypeEnvFromLetPairs env outerTypeEnv = foldl' addBinding Map.empty
  where
    addBinding acc (Var n t _, rhs) =
        case exprToTypeName t of
            Just tn -> Map.insert n tn acc
            Nothing ->
                let currentEnv = Map.union acc outerTypeEnv
                in case inferExprTypeName env currentEnv rhs of
                    Just tn -> Map.insert n tn acc
                    Nothing -> acc

-- | Extract concrete type names for pattern-bound variables from CaseOf vars.
extractPatternTypes :: Environment -> TypeEnv -> [Var] -> TypeEnv
extractPatternTypes env _outerTypeEnv caseVars =
    Map.fromList $ concatMap extractFromVar caseVars
  where
    extractFromVar (Var _scrutName varTypExpr pat) =
        let concreteTypeExpr = case varTypExpr of
                UNDEFINED -> Nothing
                other     -> Just other
        in case pat of
            App (Id consName) patArgs ->
                extractConsPattern concreteTypeExpr consName patArgs
            _ -> []

    extractConsPattern mConcreteType consName patArgs =
        case lookupConstructor consName env of
            Just (consLam, _) ->
                let implParams = filter isImplicitParam (params consLam)
                    valParams  = filter (not . isImplicitParam) (params consLam)
                    typeArgs = case mConcreteType of
                        Just (App _ args) -> args
                        _ -> []
                    typeSubst = Map.fromList $ zip (map name implParams) typeArgs
                in catMaybes
                    [ case patArg of
                        Id varName | isLowerCase varName ->
                            let fieldType = substTypeExpr typeSubst (typ fieldParam)
                            in case exprToTypeName fieldType of
                                Just tn -> Just (varName, tn)
                                Nothing -> Nothing
                        _ -> Nothing
                    | (patArg, fieldParam) <- zip patArgs valParams
                    ]
            Nothing -> []

    isImplicitParam (Var _ (Implicit _) _) = True
    isImplicitParam _ = False

    isLowerCase n = not (null n) && head n >= 'a' && head n <= 'z'

    substTypeExpr subst (Id n) = fromMaybe (Id n) (Map.lookup n subst)
    substTypeExpr subst (App f args) = App (substTypeExpr subst f) (map (substTypeExpr subst) args)
    substTypeExpr _ e = e

-- ============================================================================
-- (Unused callee-directed propagation removed — integrated into resolveImplicitCall)

-- ============================================================================
-- Type Name Inference (reads from TC-annotated AST)
-- ============================================================================

-- | Infer the concrete type name of a Surface expression.
-- After Pass 3.2 (type elaboration), most expressions have Typed wrappers,
-- so the primary path is reading from those. Fallbacks handle un-elaborated cases.
inferExprTypeName :: Environment -> TypeEnv -> Expr -> Maybe Name
inferExprTypeName env typeEnv (Typed e t) =                    -- primary: read from elaboration
    case exprToTypeName t of
        Just tn -> Just tn
        Nothing -> inferExprTypeName env typeEnv e             -- type unresolvable, try inner expr
inferExprTypeName _ _ (Lit lit) = litTypeName lit               -- fallback: literal
inferExprTypeName env typeEnv (Id n) =
    case Map.lookup n typeEnv of
        Just tn -> Just tn
        Nothing ->
            if not (null n) && head n >= 'A' && head n <= 'Z'
                then lookupTypeOfConstructor n env
                else Nothing
inferExprTypeName _ _ (ActionBlock _) = Just "Unit"
inferExprTypeName env _ (Function lam) = exprToTypeName (lamType lam)  -- lambda return type
inferExprTypeName env _ (App (Function lam) _) =                       -- applied lambda = return type
    exprToTypeName (lamType lam)
inferExprTypeName env _ (App f _)
    | Just n <- calleeHeadName f =                                     -- fallback: function return type
        case lookupConstructor n env of
            Just (lam, _) -> exprToTypeName (lamType lam)
            Nothing ->
                case lookupLambda n env of
                    Just lam -> exprToTypeName (lamType lam)
                    Nothing -> lookupExternRetType n env
inferExprTypeName env _ (ConTuple (ConsTag cname _) _) =
    lookupTypeOfConstructor cname env
inferExprTypeName env typeEnv (RecFieldAccess _ e) =
    inferExprTypeName env typeEnv e
inferExprTypeName env typeEnv (LetIn binds bodyExpr) =
    inferExprTypeName env typeEnv bodyExpr <|> firstJust [inferExprTypeName env typeEnv rhs | (_, rhs) <- binds]
inferExprTypeName env typeEnv (CaseOf _ branchExpr _) =
    inferExprTypeName env typeEnv branchExpr
inferExprTypeName env typeEnv (ExpandedCase checks branchExpr _) =
    inferExprTypeName env typeEnv branchExpr <|> firstJust [inferExprTypeName env typeEnv chk | chk <- checks]
inferExprTypeName env typeEnv (IfThenElse _ thenBr _) =
    inferExprTypeName env typeEnv thenBr
-- Phase 5.1: PatternMatches — type from first branch
inferExprTypeName env typeEnv (PatternMatches (c:_)) =
    inferExprTypeName env typeEnv c
-- Phase 5.1: NTuple — no single type name
inferExprTypeName _ _ (NTuple _) = Nothing
-- Phase 5.1: ArrayLit — Array type
inferExprTypeName _ _ (ArrayLit _) = Just "Array"
-- Phase 5.1: HandleWith — type of computation
inferExprTypeName env typeEnv (HandleWith c _) =
    inferExprTypeName env typeEnv c
-- Phase 5.1: BinaryOp — try operator return type
inferExprTypeName env _ (BinaryOp op _ _) =
    case lookupLambda op env of
        Just lam -> exprToTypeName (lamType lam)
        Nothing -> Nothing
-- Phase 5.1: UnaryOp — try operator return type
inferExprTypeName env _ (UnaryOp op _) =
    case lookupLambda op env of
        Just lam -> exprToTypeName (lamType lam)
        Nothing -> Nothing
-- Phase 5.1: ReprCast — the target type
inferExprTypeName _ _ (ReprCast _ t) = exprToTypeName t
-- Phase 5.1: Statements — type of last statement
inferExprTypeName env typeEnv (Statements stmts) =
    case reverse stmts of
        (s:_) -> inferExprTypeName env typeEnv s
        []    -> Nothing
inferExprTypeName _ _ _ = Nothing

-- | Look up the return type of a target extern function.
lookupExternRetType :: Name -> Environment -> Maybe Name
lookupExternRetType fname env =
    let allTargetExts = Map.elems (targetExterns env)
    in firstJust [case Map.lookup fname te of
            Just (_, retTy, _) -> exprToTypeName retTy
            Nothing -> Nothing
        | te <- allTargetExts]

-- ============================================================================
-- Target Instance Resolution
-- ============================================================================

resolveTargetInstanceMulti :: NameMap Lambda -> Name -> [Name] -> Maybe Lambda
resolveTargetInstanceMulti tInstances funcName types =
    case Map.lookup (mkInstanceKey funcName types Nothing) tInstances of
        Just lam -> Just lam
        Nothing ->
            let uniqueTypes = nub' types
            in case Map.lookup (mkInstanceKey funcName uniqueTypes Nothing) tInstances of
                Just lam -> Just lam
                Nothing ->
                    firstJust [Map.lookup (mkInstanceKey funcName [t] Nothing) tInstances | t <- uniqueTypes]

-- ============================================================================
-- Substitution & Utilities
-- ============================================================================

-- | Substitute an instance Lambda's parameters with actual arguments.
-- Wraps the result with the instance's return type (if known) so that
-- outer expressions can determine the type of the substituted call.
substituteInstanceBody :: Lambda -> [Expr] -> Expr
substituteInstanceBody instLam args =
    let valueParams = filter (not . isImplicitVar) (params instLam)
        subst = Map.fromList $ zip (map name valueParams) args
        result = substituteNames subst (body instLam)
    in case lamType instLam of
        UNDEFINED -> result
        retType   -> Typed result retType

-- | Substitute all Id references in an expression according to a name map.
substituteNames :: HashMap Name Expr -> Expr -> Expr
substituteNames subst = go
  where
    go (Id n) = fromMaybe (Id n) (Map.lookup n subst)
    go (App f args) = App (go f) (map go args)
    go (Lit l) = Lit l
    go (Function lam) =
        let shadowed = Map.fromList [(name v, ()) | v <- params lam]
            subst' = Map.difference subst shadowed
        in Function $ lam { body = substituteNames subst' (body lam) }
    go (LetIn binds bodyExpr) =
        let binds' = map (\(v, ex) -> (v, go ex)) binds
            shadowed = Map.fromList [(name v, ()) | (v, _) <- binds]
            subst' = Map.difference subst shadowed
        in LetIn binds' (substituteNames subst' bodyExpr)
    go (Typed e t) = Typed (go e) t
    go (IfThenElse c t e) = IfThenElse (go c) (go t) (go e)
    go (BinaryOp op l r) = BinaryOp op (go l) (go r)
    go (ConTuple ct args) = ConTuple ct (map go args)
    go (NTuple fields) = NTuple (map (\(mn, e) -> (mn, go e)) fields)
    go (ExpandedCase checks bodyExpr si) =
        ExpandedCase (map goPC checks) (go bodyExpr) si
    go (CaseOf cv ex si) = CaseOf (map (\v -> v { val = go (val v) }) cv) (go ex) si
    go (PatternMatches cases) = PatternMatches (map go cases)
    go (RecFieldAccess ac e) = RecFieldAccess ac (go e)
    go (ActionBlock stmts) = ActionBlock (map goAS stmts)
    go (ArrayLit es) = ArrayLit (map go es)
    go e = e  -- U, UNDEFINED, Intrinsic, etc.

    goPC (PatternGuard pc e) = PatternGuard pc (go e)
    goPC other = other

    goAS (ActionExpr e) = ActionExpr (go e)
    goAS (ActionBind n e) = ActionBind n (go e)
    goAS (ActionLet n e) = ActionLet n (go e)

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' (filter (/= x) xs)

-- | Brief string representation of an expression for debug output.
showExprBrief :: Expr -> String
showExprBrief (Id n) = "Id:" ++ n
showExprBrief (Lit l) = "Lit:" ++ show l
showExprBrief (App (Id n) args) = "App:" ++ n ++ "(" ++ show (length args) ++ " args)"
showExprBrief (Typed e t) = "Typed(" ++ showExprBrief e ++ ":" ++ showExprBrief t ++ ")"
showExprBrief (Function lam) = "Fn:" ++ lamName lam
showExprBrief (ConTuple (ConsTag cn _) _) = "Con:" ++ cn
showExprBrief (IfThenElse {}) = "IfThenElse"
showExprBrief (ActionBlock _) = "ActionBlock"
showExprBrief (LetIn _ _) = "LetIn"
showExprBrief (CaseOf _ _ _) = "CaseOf"
showExprBrief (ExpandedCase _ _ _) = "ExpandedCase"
showExprBrief (BinaryOp op _ _) = "BinOp:" ++ op
showExprBrief (RecFieldAccess nm _) = "RecField:" ++ show nm
showExprBrief e = take 30 (show e)

hasAnyTargetInstance :: Name -> HashMap Name a -> Bool
hasAnyTargetInstance funcName m =
    let prefix = funcName ++ "\0"
    in any (\k -> k == funcName || take (length prefix) k == prefix) (Map.keys m)

-- | Check if any instance lambda exists for a function name.
hasAnyInstanceLambda :: Name -> Environment -> Bool
hasAnyInstanceLambda funcName env =
    let instLams = instanceLambdas env
        prefix = funcName ++ "\0"
    in any (\k -> k == funcName || take (length prefix) k == prefix) (Map.keys instLams)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : rest) = firstJust rest
