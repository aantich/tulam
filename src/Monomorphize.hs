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
    ) where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.List (foldl', partition, sortOn)
import Control.Monad.Trans.State.Strict (get, modify)

import Surface
import State
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
        body' = resolveImplicitCalls debug monoLevel env tInstances handlerOps typeEnv mRetType (body lam)
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
exprToTypeName :: Expr -> Maybe Name
exprToTypeName (Typed _ t) = exprToTypeName t
exprToTypeName (Id n)
    | not (null n) && head n >= 'A' && head n <= 'Z' = Just n
exprToTypeName (App f _)
    | Just n <- calleeHeadName f
    , not (null n)
    , head n >= 'A' && head n <= 'Z' = Just n
exprToTypeName _ = Nothing

calleeHeadName :: Expr -> Maybe Name
calleeHeadName (Id n) = Just n
calleeHeadName (Typed e _) = calleeHeadName e
calleeHeadName _ = Nothing

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
                let isImplicit = case lookupLambda funcName env of
                        Just fun | hasImplicit fun -> True
                        _ -> False
                    hasTargetInstance = hasAnyTargetInstance funcName tInstances
                in if isImplicit || hasTargetInstance || Map.member funcName handlerOps
                    then resolveImplicitCall funcName (map go args)
                    else App (go f) (map go args)

        -- Nullary implicit-param reference (e.g., `zero`, `one`, `empty`, `readLine`)
        Id funcName ->
            case lookupLambda funcName env of
                Just fun | hasImplicit fun ->
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
        Function lam ->
            let innerTypeEnv = Map.union (buildTypeEnv (params lam)) typeEnv
                innerRet = case exprToTypeName (lamType lam) of
                    Just r  -> Just r
                    Nothing -> mLamRet
            in Function $ lam { body = resolveImplicitCalls debug monoLevel env tInstances handlerOps innerTypeEnv innerRet (body lam) }
        LetIn binds bodyExpr ->
            let binds' = map (\(v, ex) -> (v, go ex)) binds
                inferredEnv = buildTypeEnvFromLetPairs env typeEnv binds'
                letTypeEnv = Map.union inferredEnv typeEnv
            in LetIn binds' (resolveImplicitCalls debug monoLevel env tInstances handlerOps letTypeEnv mLamRet bodyExpr)
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
    resolveImplicitCall :: Name -> [Expr] -> Expr
    resolveImplicitCall funcName args' =
        -- Try handler operation first (putStrLn, readRef, etc.)
        case Map.lookup funcName handlerOps of
            Just handlerLam ->
                dbg debug ("resolve " ++ funcName ++ " → handler op") $
                go (substituteInstanceBody handlerLam args')
            Nothing ->
                let argTypes = map (inferExprTypeName env typeEnv) args'
                in case sequence argTypes of
                    Just types ->
                        dbg debug ("resolve " ++ funcName ++ " types=" ++ show types) $
                        resolveWithTypes funcName types args'
                    Nothing ->
                        -- Partial: try with just the known types
                        let knownTypes = nub' (catMaybes argTypes)
                        in if null knownTypes
                            then dbg debug ("UNRESOLVED " ++ funcName ++ " — no arg types inferred, args=" ++ show (map showExprBrief args')) $
                                 App (Id funcName) args'
                            else dbg debug ("resolve " ++ funcName ++ " partial types=" ++ show knownTypes) $
                                 resolveWithTypes funcName knownTypes args'

    -- | Try to resolve given known arg types, with morphism fallback.
    resolveWithTypes :: Name -> [Name] -> [Expr] -> Expr
    resolveWithTypes funcName types args' =
        -- 1. Try target instances (algebra methods)
        case resolveTargetInstanceMulti tInstances funcName types of
            Just resolvedBody ->
                dbg debug ("  " ++ funcName ++ " → target instance for " ++ show types) $
                let result = go (substituteInstanceBody resolvedBody args')
                in wrapWithReturnType funcName types result
            Nothing ->
                -- 2. Try with return type appended (for morphisms like Convertible)
                case mLamRet of
                    Just ret ->
                        case resolveTargetInstanceMulti tInstances funcName (types ++ [ret]) of
                            Just resolvedBody ->
                                dbg debug ("  " ++ funcName ++ " → target instance (morphism) for " ++ show (types ++ [ret])) $
                                let result = go (substituteInstanceBody resolvedBody args')
                                in wrapWithReturnType funcName types result
                            Nothing -> tryInstanceLambdaFallback funcName types args'
                    Nothing -> tryInstanceLambdaFallback funcName types args'

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
            -- Try with return type appended first (for morphisms)
            let withRet = case mLamRet of
                    Just ret -> resolveInstanceLambdaWithKey env funcName (argTypes ++ [ret])
                    Nothing  -> Nothing
            in case withRet of
                Just (instKey, _instLam) ->
                    dbg debug ("  " ++ funcName ++ " → instance " ++ instKey ++ " (morphism fallback)") $
                    App (Id instKey) args'
                _ ->
                    case resolveInstanceLambdaWithKey env funcName argTypes of
                        Just (instKey, _instLam) ->
                            dbg debug ("  " ++ funcName ++ " → instance " ++ instKey) $
                            App (Id instKey) args'
                        Nothing ->
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
inferExprTypeName env typeEnv (IfThenElse _ thenBr _) =
    inferExprTypeName env typeEnv thenBr
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

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : rest) = firstJust rest
