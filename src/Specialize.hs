{-# LANGUAGE OverloadedStrings #-}
-- | Type specialization pass: creates concrete copies of generic instance functions.
--
-- Runs AFTER monomorphization (dispatch resolution) and BEFORE CLM conversion.
-- The monomorphizer resolves `show(42)` → `show\0Int(42)`, but generic instances
-- like `show\0List` still have unresolved inner calls (`show(head)` where `head:a`).
--
-- Specialization fixes this by:
--   1. Finding calls to generic instances (e.g., `show\0List(myIntList)`)
--   2. Extracting concrete type args from the call site (`a=Int`)
--   3. Cloning the instance body with type args substituted
--   4. Re-running dispatch resolution on the clone
--   5. Registering the specialized copy with a new key (`show\0List\0$Int`)
--   6. Repeating until fixed point
--
-- Architecture supports future SpecSmart (selective) and SpecNone (dispatch tables).
module Specialize
    ( specializeLambdas
    , parseInstanceKey
    , mkSpecKey
    ) where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HSet
import Data.HashSet (HashSet)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)

import Surface
import State
import Monomorphize (resolveImplicitCalls, inferExprTypeName)
import qualified Debug.Trace as DT

-- | Conditional debug trace (mirrors Monomorphize.dbg).
dbg :: Bool -> String -> a -> a
dbg False _ x = x
dbg True msg x = DT.trace ("[spec] " ++ msg) x

-- ============================================================================
-- Public API
-- ============================================================================

-- | Specialize generic instance functions for concrete type arguments.
-- Returns updated (user functions, instance functions) with specialized copies added
-- and call sites rewritten to use them.
specializeLambdas :: Bool                    -- ^ Debug tracing
                  -> Name                    -- ^ Target name (e.g., "native")
                  -> Environment             -- ^ Full environment for lookups
                  -> HashMap Name Lambda     -- ^ Monomorphized user functions
                  -> HashMap Name Lambda     -- ^ Monomorphized instance functions
                  -> (HashMap Name Lambda, HashMap Name Lambda)
specializeLambdas debug targetName env userFuncs instFuncs =
    let tInstances = fromMaybe Map.empty (Map.lookup targetName (targetInstances env))
        handlerOps = buildHandlerOpsMapForSpec targetName env
        -- Merge all instances (env + monomorphized) for lookup.
        -- Prefer elaborated instances (with Typed wrappers) when available.
        allInstances = Map.union instFuncs (instanceLambdasByRep ElabLambdaRep env)
        initState = SpecState
            { ssSpecialized = Map.empty
            , ssVisited = HSet.empty
            , ssAllInstances = allInstances
            , ssTInstances = tInstances
            , ssHandlerOps = handlerOps
            , ssEnv = env
            , ssDebug = debug
            , ssTargetName = targetName
            }
        -- Run fixed-point specialization
        finalState = fixpoint 0 50 initState (Map.union userFuncs instFuncs)
        -- Rewrite call sites: replace generic instance calls with specialized versions
        -- where the concrete type args match
        rewriter = rewriteCallSitesWithSpec (ssDebug initState) (ssEnv initState)
                     (ssAllInstances finalState)
        userFuncs' = Map.map rewriter userFuncs
        instFuncs' = Map.map rewriter instFuncs
        -- Also rewrite within the new specialized instances themselves
        specInsts = Map.map rewriter (ssSpecialized finalState)
        -- Merge specialized instances into the instance set
        allInsts = Map.union specInsts instFuncs'
    in (userFuncs', allInsts)

-- ============================================================================
-- Specialization State
-- ============================================================================

data SpecState = SpecState
    { ssSpecialized  :: HashMap Name Lambda   -- newly created specialized instances
    , ssVisited      :: HashSet Name          -- spec keys already processed (cycle detection)
    , ssAllInstances :: HashMap Name Lambda   -- all instance lambdas for lookup
    , ssTInstances   :: HashMap Name Lambda   -- target instances
    , ssHandlerOps   :: HashMap Name Lambda   -- handler ops
    , ssEnv          :: Environment
    , ssDebug        :: Bool
    , ssTargetName   :: Name
    }

-- ============================================================================
-- Fixed-Point Loop
-- ============================================================================

-- | Run specialization rounds until no new specialized instances are produced,
-- or we hit the iteration limit.
fixpoint :: Int -> Int -> SpecState -> HashMap Name Lambda -> SpecState
fixpoint round maxRounds st allFuncs
    | round >= maxRounds =
        dbg (ssDebug st) ("WARNING: specialization hit iteration limit (" ++ show maxRounds ++ ")") st
    | otherwise =
        let -- Collect specialization candidates from all function bodies
            candidates = concatMap (collectSpecCandidates st) (Map.toList allFuncs)
            -- Filter out already-visited
            newCandidates = filter (\(specKey, _, _) -> not (HSet.member specKey (ssVisited st))) candidates
        in if null newCandidates
            then dbg (ssDebug st) ("specialization complete after " ++ show round ++ " round(s), "
                     ++ show (Map.size (ssSpecialized st)) ++ " specialized instances") st
            else
                let st' = foldl specializeOne st newCandidates
                    -- Add newly created instances to the scan set for next round
                    newFuncs = Map.union (ssSpecialized st') allFuncs
                in fixpoint (round + 1) maxRounds st' newFuncs

-- | Specialize one candidate: clone instance body, substitute type args, re-resolve.
specializeOne :: SpecState -> (Name, Name, HashMap Name Expr) -> SpecState
specializeOne st (specKey, instKey, typeSubst) =
    case Map.lookup instKey (ssAllInstances st) of
        Nothing -> st  -- instance not found (shouldn't happen)
        Just instLam ->
            let debug = ssDebug st
                -- Substitute type variables in the instance body
                substBody = substituteTypeVarsInExpr typeSubst (body instLam)
                -- Build type env from the substituted params
                substParams = map (substituteTypeVarsInVar typeSubst) (params instLam)
                typeEnv = buildTypeEnvFromParams substParams
                -- Re-run dispatch resolution on the substituted body
                resolved = resolveImplicitCalls debug MonoFull (ssEnv st) (ssTInstances st)
                             (ssHandlerOps st) typeEnv Nothing substBody
                -- Create the specialized lambda
                specLam = instLam { lamName = specKey, body = resolved, params = substParams }
            in dbg debug ("specialized " ++ instKey ++ " → " ++ specKey
                         ++ " subst=" ++ show (Map.toList typeSubst)) $
               st { ssSpecialized = Map.insert specKey specLam (ssSpecialized st)
                  , ssVisited = HSet.insert specKey (ssVisited st)
                  , ssAllInstances = Map.insert specKey specLam (ssAllInstances st)
                  }

-- ============================================================================
-- Candidate Collection
-- ============================================================================

-- | Scan a function body for calls to instance keys that need specialization.
-- Returns (specKey, instanceKey, typeSubst) triples.
collectSpecCandidates :: SpecState -> (Name, Lambda) -> [(Name, Name, HashMap Name Expr)]
collectSpecCandidates st (funcName, lam) =
    let typeEnv = buildTypeEnvFromParams (params lam)
        result = collectFromExpr st typeEnv (body lam)
    in dbg (ssDebug st && not (null result)) ("scanning " ++ funcName ++ " found " ++ show (length result) ++ " candidates") result

collectFromExpr :: SpecState -> HashMap Name Name -> Expr -> [(Name, Name, HashMap Name Expr)]
collectFromExpr st typeEnv expr = case expr of
    -- The key case: call to an instance function
    App (Id funcName) args ->
        let isInstKey = '\0' `elem` funcName
            selfCandidates = if isInstKey
                then dbg (ssDebug st) ("scan call " ++ show funcName ++ " (instKey=" ++ show isInstKey ++ ", args=" ++ show (length args) ++ ")") $
                     checkCallSite st typeEnv funcName args
                else []
            childCandidates = concatMap (collectFromExpr st typeEnv) args
        in selfCandidates ++ childCandidates

    -- Desugared let-in: extend type env
    App (Function lam) args ->
        let paramEnv = buildTypeEnvFromParams (params lam)
            innerEnv = Map.union paramEnv typeEnv
        in concatMap (collectFromExpr st typeEnv) args
           ++ collectFromExpr st innerEnv (body lam)

    App f args -> collectFromExpr st typeEnv f ++ concatMap (collectFromExpr st typeEnv) args
    Function lam ->
        let innerEnv = Map.union (buildTypeEnvFromParams (params lam)) typeEnv
        in collectFromExpr st innerEnv (body lam)
    LetIn binds bodyExpr ->
        concatMap (collectFromExpr st typeEnv . snd) binds
        ++ collectFromExpr st typeEnv bodyExpr
    ExpandedCase checks bodyExpr _ ->
        concatMap (\c -> case c of PatternGuard _ e -> collectFromExpr st typeEnv e; _ -> []) checks
        ++ collectFromExpr st typeEnv bodyExpr
    CaseOf cv ex _ ->
        concatMap (collectFromExpr st typeEnv . val) cv ++ collectFromExpr st typeEnv ex
    PatternMatches cases -> concatMap (collectFromExpr st typeEnv) cases
    ConTuple _ args -> concatMap (collectFromExpr st typeEnv) args
    NTuple fields -> concatMap (collectFromExpr st typeEnv . snd) fields
    ActionBlock stmts -> concatMap (collectFromActionStmt st typeEnv) stmts
    IfThenElse c t e -> collectFromExpr st typeEnv c ++ collectFromExpr st typeEnv t ++ collectFromExpr st typeEnv e
    BinaryOp _ l r -> collectFromExpr st typeEnv l ++ collectFromExpr st typeEnv r
    Typed e _ -> collectFromExpr st typeEnv e
    RecFieldAccess _ e -> collectFromExpr st typeEnv e
    ReprCast e _ -> collectFromExpr st typeEnv e
    ArrayLit es -> concatMap (collectFromExpr st typeEnv) es
    _ -> []

collectFromActionStmt :: SpecState -> HashMap Name Name -> ActionStmt -> [(Name, Name, HashMap Name Expr)]
collectFromActionStmt st typeEnv (ActionExpr e) = collectFromExpr st typeEnv e
collectFromActionStmt st typeEnv (ActionBind _ e) = collectFromExpr st typeEnv e
collectFromActionStmt st typeEnv (ActionLet _ e) = collectFromExpr st typeEnv e

-- | Check if a call site is a specialization candidate.
-- A call like `App (Id "show\0List") [arg]` is a candidate if:
--   1. The callee is an instance key (contains \0)
--   2. The instance Lambda has implicit (type) params
--   3. We can infer concrete type args from the actual arguments
checkCallSite :: SpecState -> HashMap Name Name -> Name -> [Expr] -> [(Name, Name, HashMap Name Expr)]
checkCallSite st typeEnv funcName args
    | '\0' `notElem` funcName = []  -- not an instance key
    | otherwise =
        case Map.lookup funcName (ssAllInstances st) of
            Nothing ->
                dbg (ssDebug st) ("candidate " ++ show funcName ++ " — instance not found in allInstances ("
                    ++ show (length (Map.keys (ssAllInstances st))) ++ " keys)") []
            Just instLam ->
                let implicitParams = filter isImplicitVar (params instLam)
                    valueParams = filter (not . isImplicitVar) (params instLam)
                    debug = ssDebug st
                in if null implicitParams
                    then dbg debug ("candidate " ++ show funcName ++ " — found but no implicit params"
                        ++ ", params=" ++ show (map (\v -> (name v, show (typ v))) (params instLam))) []
                    else
                        -- Try to extract concrete types from call site args
                        case inferTypeSubst (ssEnv st) typeEnv implicitParams valueParams args of
                            Nothing ->
                                dbg debug ("candidate " ++ funcName ++ " — could not infer type subst"
                                    ++ ", implParams=" ++ show (map name implicitParams)
                                    ++ ", valParams=" ++ show (map (\v -> (name v, show (typ v))) valueParams)
                                    ++ ", argCount=" ++ show (length args)) []
                            Just typeSubst ->
                                let specTypes = map (\v -> case Map.lookup (name v) typeSubst of
                                        Just (Id n) -> n
                                        Just (App (Id n) _) -> n
                                        _ -> "?") implicitParams
                                    specKey = mkSpecKey funcName specTypes
                                in if HSet.member specKey (ssVisited st) || Map.member specKey (ssAllInstances st)
                                    then dbg debug ("candidate " ++ funcName ++ " → " ++ specKey ++ " already exists") []
                                    else [(specKey, funcName, typeSubst)]

-- ============================================================================
-- Type Inference at Call Sites
-- ============================================================================

-- | Infer the type substitution by matching actual args against instance param types.
-- Given implicit params [a:Type], value params [x:List(a)], and actual args [someList],
-- infers {a → Int} by matching List(a) against the inferred type of someList (List(Int)).
inferTypeSubst :: Environment -> HashMap Name Name
               -> [Var]          -- implicit (type) params
               -> [Var]          -- value params
               -> [Expr]         -- actual arguments
               -> Maybe (HashMap Name Expr)
inferTypeSubst env typeEnv implicitParams valueParams args =
    let -- Get implicit param names (the type variables)
        typeVarNames = HSet.fromList (map name implicitParams)
        -- For each (value param, actual arg) pair, try to extract type info
        bindings = catMaybes
            [ matchType env typeEnv typeVarNames (typ vparam) arg
            | (vparam, arg) <- zip valueParams args
            ]
        merged = foldl (Map.unionWith const) Map.empty bindings
    in if Map.null merged then Nothing else Just merged

-- | Match a param type pattern against an actual argument to extract type variable bindings.
-- E.g., matching `List(a)` against an arg of type `List(Int)` yields {a → Id "Int"}.
matchType :: Environment -> HashMap Name Name -> HashSet Name -> Expr -> Expr -> Maybe (HashMap Name Expr)
matchType env typeEnv typeVars paramType arg =
    case inferExprTypeExpr env typeEnv arg of
        Nothing -> Nothing
        Just argType -> unifyTypes typeVars paramType argType

-- | Infer the full type expression of a Surface expression.
-- After Pass 3.2 (type elaboration), most expressions have Typed wrappers.
-- Falls back to literal/variable lookup for un-elaborated cases.
inferExprTypeExpr :: Environment -> HashMap Name Name -> Expr -> Maybe Expr
inferExprTypeExpr _ _ (Typed _ t) = Just t                     -- primary: read from elaboration
inferExprTypeExpr _ _ (Lit lit) = fmap Id (litTypeName lit)    -- fallback: literal
inferExprTypeExpr _ typeEnv (Id n) =
    case Map.lookup n typeEnv of
        Just tn -> Just (Id tn)
        Nothing -> Nothing
inferExprTypeExpr _ _ (ActionBlock _) = Just (Id "Unit")
-- Fallback for un-elaborated constructor/function applications
inferExprTypeExpr env typeEnv (App (Id n) args) =
    case lookupConstructor n env of
        Just (lam, _) ->
            let consType = lamType lam
                implParams = filter isImplicitVar (params lam)
                valParams = filter (not . isImplicitVar) (params lam)
                typeVarNames = HSet.fromList (map name implParams)
                bindings = catMaybes
                    [ case inferExprTypeExpr env typeEnv arg of
                        Just argTy -> unifyTypes typeVarNames (typ vp) argTy
                        Nothing -> Nothing
                    | (vp, arg) <- zip valParams args
                    ]
                subst = foldl (Map.unionWith const) Map.empty bindings
            in Just (applyTypeSubst subst consType)
        Nothing ->
            case lookupLambda n env of
                Just lam -> case lamType lam of
                    UNDEFINED -> Nothing
                    t -> Just t
                Nothing -> Nothing
inferExprTypeExpr _ _ _ = Nothing

-- | Simple structural unification to extract type variable bindings.
-- Matches a pattern type (with type vars) against a concrete type.
unifyTypes :: HashSet Name -> Expr -> Expr -> Maybe (HashMap Name Expr)
unifyTypes typeVars (Id n) concrete
    | HSet.member n typeVars = Just (Map.singleton n concrete)
    | otherwise = case concrete of
        Id m | n == m -> Just Map.empty
        _ -> Nothing
unifyTypes typeVars (App (Id pn) pargs) (App (Id cn) cargs)
    | pn == cn && length pargs == length cargs =
        let bindings = catMaybes [unifyTypes typeVars pa ca | (pa, ca) <- zip pargs cargs]
        in Just (foldl (Map.unionWith const) Map.empty bindings)
unifyTypes _ _ _ = Nothing

-- | Apply a type substitution to a type expression.
applyTypeSubst :: HashMap Name Expr -> Expr -> Expr
applyTypeSubst subst (Id n) = fromMaybe (Id n) (Map.lookup n subst)
applyTypeSubst subst (App f args) = App (applyTypeSubst subst f) (map (applyTypeSubst subst) args)
applyTypeSubst _ e = e

-- ============================================================================
-- Type Variable Substitution in Lambda Bodies
-- ============================================================================

-- | Substitute type variables in a Var's type field.
substituteTypeVarsInVar :: HashMap Name Expr -> Var -> Var
substituteTypeVarsInVar subst v = v { typ = applyTypeSubst subst (typ v) }

-- | Substitute type variables in expression type positions.
-- This replaces type variable references in Typed annotations and constructor type args.
substituteTypeVarsInExpr :: HashMap Name Expr -> Expr -> Expr
substituteTypeVarsInExpr subst = go
  where
    go (Typed e t) = Typed (go e) (applyTypeSubst subst t)
    go (App f args) = App (go f) (map go args)
    go (Function lam) = Function $ lam
        { body = go (body lam)
        , params = map (substituteTypeVarsInVar subst) (params lam)
        , lamType = applyTypeSubst subst (lamType lam)
        }
    go (LetIn binds bodyExpr) =
        LetIn (map (\(v, e) -> (substituteTypeVarsInVar subst v, go e)) binds) (go bodyExpr)
    go (ExpandedCase checks bodyExpr si) =
        ExpandedCase (map goPC checks) (go bodyExpr) si
    go (CaseOf cv ex si) =
        CaseOf (map (\v -> v { typ = applyTypeSubst subst (typ v), val = go (val v) }) cv) (go ex) si
    go (PatternMatches cases) = PatternMatches (map go cases)
    go (ConTuple ct args) = ConTuple ct (map go args)
    go (NTuple fields) = NTuple (map (\(mn, e) -> (mn, go e)) fields)
    go (ActionBlock stmts) = ActionBlock (map goAS stmts)
    go (IfThenElse c t e) = IfThenElse (go c) (go t) (go e)
    go (BinaryOp op l r) = BinaryOp op (go l) (go r)
    go (RecFieldAccess ac e) = RecFieldAccess ac (go e)
    go (ReprCast e t) = ReprCast (go e) (applyTypeSubst subst t)
    go (ArrayLit es) = ArrayLit (map go es)
    go e = e

    goPC (PatternGuard pc e) = PatternGuard pc (go e)
    goPC other = other

    goAS (ActionExpr e) = ActionExpr (go e)
    goAS (ActionBind n e) = ActionBind n (go e)
    goAS (ActionLet n e) = ActionLet n (go e)

-- ============================================================================
-- Call Site Rewriting
-- ============================================================================

-- | Rewrite call sites in a Lambda to use specialized instances.
-- For each call like `App (Id "show\0List") [arg]`, checks if a specialized
-- version exists for the concrete arg types and rewrites to use it.
rewriteCallSitesWithSpec :: Bool -> Environment -> HashMap Name Lambda -> Lambda -> Lambda
rewriteCallSitesWithSpec debug env allInstances lam =
    let typeEnv = buildTypeEnvFromParams (params lam)
    in lam { body = rewriteExpr debug env allInstances typeEnv (body lam) }

rewriteExpr :: Bool -> Environment -> HashMap Name Lambda -> HashMap Name Name -> Expr -> Expr
rewriteExpr debug env allInstances typeEnv = go
  where
    go expr = case expr of
        App (Id funcName) args
            | '\0' `elem` funcName ->
                let args' = map go args
                in case tryRewrite funcName args' of
                    Just newExpr -> newExpr
                    Nothing -> App (Id funcName) args'
        App (Function lam) args ->
            let args' = map go args
                paramEnv = buildTypeEnvFromParams (params lam)
                innerEnv = Map.union paramEnv typeEnv
                body' = rewriteExpr debug env allInstances innerEnv (body lam)
            in App (Function (lam { body = body' })) args'
        App f args -> App (go f) (map go args)
        Function lam ->
            let innerEnv = Map.union (buildTypeEnvFromParams (params lam)) typeEnv
                body' = rewriteExpr debug env allInstances innerEnv (body lam)
            in Function (lam { body = body' })
        LetIn binds bodyExpr -> LetIn (map (\(v, e) -> (v, go e)) binds) (go bodyExpr)
        ExpandedCase checks bodyExpr si ->
            ExpandedCase (map (\c -> case c of PatternGuard pc e -> PatternGuard pc (go e); o -> o) checks) (go bodyExpr) si
        CaseOf cv ex si ->
            CaseOf (map (\v -> v { val = go (val v) }) cv) (go ex) si
        PatternMatches cases -> PatternMatches (map go cases)
        ConTuple ct args -> ConTuple ct (map go args)
        NTuple fields -> NTuple (map (\(mn, e) -> (mn, go e)) fields)
        ActionBlock stmts -> ActionBlock (map goAS stmts)
        IfThenElse c t e -> IfThenElse (go c) (go t) (go e)
        BinaryOp op l r -> BinaryOp op (go l) (go r)
        Typed e t -> Typed (go e) t
        RecFieldAccess ac e -> RecFieldAccess ac (go e)
        ReprCast e t -> ReprCast (go e) t
        ArrayLit es -> ArrayLit (map go es)
        _ -> expr

    goAS (ActionExpr e) = ActionExpr (go e)
    goAS (ActionBind n e) = ActionBind n (go e)
    goAS (ActionLet n e) = ActionLet n (go e)

    tryRewrite funcName args' =
        case Map.lookup funcName allInstances of
            Nothing -> Nothing
            Just instLam ->
                let implParams = filter isImplicitVar (params instLam)
                    valParams = filter (not . isImplicitVar) (params instLam)
                in if null implParams then Nothing
                   else case inferTypeSubst env typeEnv implParams valParams args' of
                       Nothing -> Nothing
                       Just typeSubst ->
                           let specTypes = map (\v -> case Map.lookup (name v) typeSubst of
                                   Just (Id n) -> n
                                   Just (App (Id n) _) -> n
                                   _ -> "?") implParams
                               specKey = mkSpecKey funcName specTypes
                           in if Map.member specKey allInstances
                               then dbg debug ("rewrite " ++ funcName ++ " → " ++ specKey) $
                                    Just (App (Id specKey) args')
                               else Nothing

-- ============================================================================
-- Instance Key Parsing and Construction
-- ============================================================================

-- | Parse an instance key into its components.
-- "show\0List" → ("show", ["List"], Nothing)
-- "combine\0Int\0@Additive" → ("combine", ["Int"], Just "Additive")
-- "show\0List\0$Int" → ("show", ["List"], Nothing) with spec suffix stripped
parseInstanceKey :: Name -> Maybe (Name, [Name], Maybe Name)
parseInstanceKey key =
    case splitOn '\0' key of
        [] -> Nothing
        [_] -> Nothing  -- no \0 separator
        (funcName:rest) ->
            let (typeNames, mTag) = extractTag rest
            in Just (funcName, typeNames, mTag)
  where
    extractTag parts =
        case break (\p -> take 1 p == "@") parts of
            (types, []) -> (types, Nothing)
            (types, (tagPart:_)) -> (types, Just (drop 1 tagPart))

-- | Create a specialization key by appending $-prefixed type args.
-- mkSpecKey "show\0List" ["Int"] → "show\0List\0$Int"
-- mkSpecKey "combine\0List\0@Additive" ["Int"] → "combine\0List\0@Additive\0$Int"
mkSpecKey :: Name -> [Name] -> Name
mkSpecKey baseKey specTypes = baseKey ++ concatMap (\t -> "\0$" ++ t) specTypes

-- ============================================================================
-- Utilities
-- ============================================================================

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn sep s =
    let (part, rest) = break (== sep) s
    in part : case rest of
        [] -> []
        (_:rs) -> splitOn sep rs

-- | Build type env from params (same as Monomorphize.buildTypeEnv but local).
buildTypeEnvFromParams :: [Var] -> HashMap Name Name
buildTypeEnvFromParams = Map.fromList . mapMaybe extract
  where
    extract (Var _ (Implicit _) _) = Nothing
    extract (Var n t _) = case exprToTypeName' t of
        Just tn -> Just (n, tn)
        Nothing -> Nothing

-- | Extract concrete type name from type expr.
exprToTypeName' :: Expr -> Maybe Name
exprToTypeName' (Id n)
    | not (null n) && head n >= 'A' && head n <= 'Z' = Just n
exprToTypeName' (App (Id n) _)
    | not (null n) && head n >= 'A' && head n <= 'Z' = Just n
exprToTypeName' _ = Nothing

-- | Build handler ops map (mirrors Monomorphize.buildHandlerOpsMap).
buildHandlerOpsMapForSpec :: Name -> Environment -> HashMap Name Lambda
buildHandlerOpsMapForSpec targetName env =
    case Map.lookup targetName (targetHandlers env) of
        Nothing -> Map.empty
        Just handlers ->
            Map.fromList
                [ (lamName lam, lam)
                | (_handlerName, (_effectName, _params, impls)) <- Map.toList handlers
                , Function lam <- impls
                ]

-- isImplicitVar is imported from Surface
