{-# LANGUAGE OverloadedStrings #-}
-- | Backend-agnostic compilation driver.
--
-- Provides reachability analysis and compilation plan building that respects
-- the module system. Any backend (LLVM, JS, .NET) consumes a CompilationPlan
-- rather than operating on the raw Environment.
--
-- The driver:
--   1. Computes the transitive closure of functions reachable from entry points
--   2. Conservatively includes instances for implicit-param functions
--   3. Monomorphizes only the reachable set (not the entire 500+ function env)
--   4. Collects only the extern references actually used
--   5. Tracks module provenance for each function (for cache invalidation)
module CompileDriver
    ( CompilationPlan(..)
    , buildCompilationPlan
    , computeReachableSet
    , collectSurfaceRefs
    , collectExternRefs
    , buildOriginMap
    ) where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HSet
import Data.HashSet (HashSet)
import qualified Data.Set as Set

import Surface
import State
import Monomorphize (monomorphizeLambdas)

-- | A compilation plan: the minimal set of functions, instances, externs,
-- and types needed to compile the given entry points.
-- Produced by the CompileDriver, consumed by any backend.
data CompilationPlan = CompilationPlan
    { cpFunctions    :: HashMap Name Lambda  -- ^ Reachable top-level functions (monomorphized)
    , cpInstances    :: HashMap Name Lambda  -- ^ Reachable instance functions (monomorphized)
    , cpExternRefs   :: HashSet Name         -- ^ Extern functions actually referenced
    , cpConstructors :: HashSet Name         -- ^ Constructors needed
    , cpTypes        :: HashSet Name         -- ^ Types needed
    , cpEntryPoints  :: [Name]              -- ^ User-specified entry points
    , cpModuleOrigin :: HashMap Name String  -- ^ Function name → source module key
    } deriving (Show)

-- | Build a compilation plan for the given entry points and target.
-- This is the main entry point for any backend.
buildCompilationPlan :: [Name]             -- ^ Entry point function names
                     -> Name               -- ^ Target name (e.g. "native")
                     -> Environment        -- ^ Full loaded environment
                     -> InterpreterState   -- ^ For module provenance
                     -> CompilationPlan
buildCompilationPlan entryPoints targetName env state =
    let -- Phase A: Reachability analysis (pre-monomorphization)
        -- Pass target name so reachability prefers target instances over interpreter instances
        (reachFuncs, reachInsts, reachCons, reachTypes) =
            computeReachableSet entryPoints targetName env

        -- Filter to only reachable names
        reachableLams = Map.filterWithKey (\k _ -> HSet.member k reachFuncs) (topLambdas env)
        reachableInsts = Map.filterWithKey (\k _ -> HSet.member k reachInsts) (instanceLambdas env)

        -- Monomorphize only reachable functions, using FULL env for lookups.
        -- The full env is needed so lookupLambda can find dispatch templates like "+"
        (monoLams, monoInsts) = monomorphizeLambdas targetName env reachableLams reachableInsts

        -- Phase B: Collect extern refs from monomorphized bodies
        targetExts = case Map.lookup targetName (targetExterns env) of
            Just te -> te
            Nothing -> Map.empty
        externRefs = collectExternRefs monoLams monoInsts targetExts

        -- Module provenance
        allReachableNames = Map.keys reachableLams ++ Map.keys reachableInsts
        origin = buildOriginMap allReachableNames
                    (loadedModules (currentModuleEnv state))

    in CompilationPlan
        { cpFunctions    = Map.filter (not . hasImplicit) monoLams
        , cpInstances    = monoInsts
        , cpExternRefs   = externRefs
        , cpConstructors = reachCons
        , cpTypes        = reachTypes
        , cpEntryPoints  = entryPoints
        , cpModuleOrigin = origin
        }

-- | Compute the transitive closure of names reachable from entry points.
-- Uses target-specific instances when available (not all interpreter instances).
-- Returns (reachable functions, reachable instance keys, constructors, types).
computeReachableSet :: [Name] -> Name -> Environment
                    -> (HashSet Name, HashSet Name, HashSet Name, HashSet Name)
computeReachableSet entryPoints targetName env =
    go HSet.empty HSet.empty HSet.empty HSet.empty entryPoints
  where
    -- Target instances for the specific backend (e.g., "native")
    tInstMap = case Map.lookup targetName (targetInstances env) of
        Just ti -> ti
        Nothing -> Map.empty

    -- Handler operations: effect op name → Lambda body
    handlerOpsMap = case Map.lookup targetName (targetHandlers env) of
        Nothing -> Map.empty
        Just handlers ->
            Map.fromList
                [ (lamName lam, lam)
                | (_handlerName, (_effectName, _params, impls)) <- Map.toList handlers
                , Function lam <- impls
                ]

    go funcs insts cons typs [] = (funcs, insts, cons, typs)
    go funcs insts cons typs (name:rest)
        | HSet.member name funcs = go funcs insts cons typs rest
        | HSet.member name insts = go funcs insts cons typs rest
        | otherwise =
            case lookupLambda name env of
                Just lam | not (hasImplicit lam) ->
                    -- Concrete function: add it, collect its callees
                    let funcs' = HSet.insert name funcs
                        refs = collectSurfaceRefs env (body lam)
                        paramRefs = concatMap (collectSurfaceRefs env . val) (params lam)
                        newWork = filter (notVisited funcs' insts) (refs ++ paramRefs)
                    in go funcs' insts cons typs (newWork ++ rest)

                Just _lam ->
                    -- Implicit-param function (dispatch template like +, show, ==, effectSeq)
                    -- Check handler ops first (effectSeq, putStrLn, etc.)
                    let handlerBodyRefs = case Map.lookup name handlerOpsMap of
                            Just hLam -> collectSurfaceRefs env (body hLam)
                            Nothing -> []
                    -- Then check target instances (algebra methods like +, show)
                        targetInstKeys = findInstanceKeysForFunc name tInstMap
                        targetBodyRefs = concatMap
                            (\k -> case Map.lookup k tInstMap of
                                Just lam -> collectSurfaceRefs env (body lam)
                                Nothing -> [])
                            targetInstKeys
                        newWork = filter (notVisited funcs insts)
                                    (handlerBodyRefs ++ targetBodyRefs)
                    in go funcs insts cons typs (newWork ++ rest)

                Nothing ->
                    -- Not a top-level function. Check if it's an instance key.
                    case Map.lookup name (instanceLambdas env) of
                        Just lam ->
                            let insts' = HSet.insert name insts
                                refs = collectSurfaceRefs env (body lam)
                                newWork = filter (notVisited funcs insts') refs
                            in go funcs insts' cons typs (newWork ++ rest)
                        Nothing ->
                            -- Check if it's a constructor
                            case lookupConstructor name env of
                                Just (lam, _) ->
                                    let cons' = HSet.insert name cons
                                        typName = case lamType lam of
                                            Id tn -> HSet.insert tn typs
                                            App (Id tn) _ -> HSet.insert tn typs
                                            _ -> typs
                                    in go funcs insts cons' typName rest
                                Nothing ->
                                    -- Unknown name (extern, builtin, etc.) — skip
                                    go funcs insts cons typs rest

    notVisited funcs insts n = not (HSet.member n funcs) && not (HSet.member n insts)

-- | Find all instance keys in a map that are for a given function name.
-- Instance keys are "funcName\0type1\0type2", so we check the prefix.
findInstanceKeysForFunc :: Name -> HashMap Name a -> [Name]
findInstanceKeysForFunc funcName m =
    let prefix = funcName ++ "\0"
    in [k | k <- Map.keys m, funcName == k || take (length prefix) k == prefix]

-- | Collect all function/constructor name references from a Surface expression.
-- This is a simple recursive walk that finds all Id references and call targets.
collectSurfaceRefs :: Environment -> Expr -> [Name]
collectSurfaceRefs env = go
  where
    go (Id n) = [n]
    go (App (Id n) args) = n : concatMap go args
    go (App f args) = go f ++ concatMap go args
    go (Function lam) = go (body lam)
    go (LetIn binds bodyExpr) = concatMap (go . snd) binds ++ go bodyExpr
    go (Typed e _) = go e
    go (ExpandedCase checks bodyExpr _) = concatMap goPC checks ++ go bodyExpr
    go (PatternMatches cases) = concatMap go cases
    go (CaseOf cv ex _) = concatMap (go . val) cv ++ go ex
    go (ConTuple (ConsTag cn _) args) = cn : concatMap go args
    go (NTuple fields) = concatMap (go . snd) fields
    go (DeclBlock exprs) = concatMap go exprs
    go (ActionBlock stmts) = concatMap goAS stmts
    go (IfThenElse c t e) = go c ++ go t ++ go e
    go (BinaryOp _ l r) = go l ++ go r
    go (RecFieldAccess _ e) = go e
    go (ReprCast e _) = go e
    go (ArrayLit es) = concatMap go es
    go (Instance _ _ impls _) = concatMap go impls
    go _ = []  -- Lit, U, UNDEFINED, Intrinsic, Implicit, etc.

    goPC (PatternGuard _ e) = go e
    goPC _ = []

    goAS (ActionExpr e) = go e
    goAS (ActionBind _ e) = go e
    goAS (ActionLet _ e) = go e

-- | Collect extern function references from monomorphized lambda bodies.
-- After monomorphization, implicit-param calls have been resolved to extern names
-- (like __add_i64). This function finds which externs are actually used.
collectExternRefs :: HashMap Name Lambda    -- ^ Monomorphized top-level functions
                  -> HashMap Name Lambda    -- ^ Monomorphized instance functions
                  -> HashMap Name a         -- ^ Target extern declarations
                  -> HashSet Name
collectExternRefs topLams instLams targetExts =
    let allBodies = map body (Map.elems topLams) ++ map body (Map.elems instLams)
        allRefs = concatMap (collectBodyRefs) allBodies
    in HSet.fromList [r | r <- allRefs, Map.member r targetExts]
  where
    -- Simpler ref collection — just find all Id/App targets
    collectBodyRefs (Id n) = [n]
    collectBodyRefs (App (Id n) args) = n : concatMap collectBodyRefs args
    collectBodyRefs (App f args) = collectBodyRefs f ++ concatMap collectBodyRefs args
    collectBodyRefs (Function lam) = collectBodyRefs (body lam)
    collectBodyRefs (LetIn binds bodyExpr) = concatMap (collectBodyRefs . snd) binds ++ collectBodyRefs bodyExpr
    collectBodyRefs (Typed e _) = collectBodyRefs e
    collectBodyRefs (ExpandedCase checks bodyExpr _) =
        concatMap goPC' checks ++ collectBodyRefs bodyExpr
    collectBodyRefs (PatternMatches cases) = concatMap collectBodyRefs cases
    collectBodyRefs (CaseOf cv ex _) = concatMap (collectBodyRefs . val) cv ++ collectBodyRefs ex
    collectBodyRefs (ConTuple _ args) = concatMap collectBodyRefs args
    collectBodyRefs (NTuple fields) = concatMap (collectBodyRefs . snd) fields
    collectBodyRefs (IfThenElse c t e) = collectBodyRefs c ++ collectBodyRefs t ++ collectBodyRefs e
    collectBodyRefs (BinaryOp _ l r) = collectBodyRefs l ++ collectBodyRefs r
    collectBodyRefs (ActionBlock stmts) = concatMap goAS' stmts
    collectBodyRefs (RecFieldAccess _ e) = collectBodyRefs e
    collectBodyRefs (ArrayLit es) = concatMap collectBodyRefs es
    collectBodyRefs _ = []

    goPC' (PatternGuard _ e) = collectBodyRefs e
    goPC' _ = []

    goAS' (ActionExpr e) = collectBodyRefs e
    goAS' (ActionBind _ e) = collectBodyRefs e
    goAS' (ActionLet _ e) = collectBodyRefs e

-- | Build a map from function names to their source module key.
-- Scans loadedModules to find which module's name set contains each function.
buildOriginMap :: [Name] -> HashMap String ModuleEnv -> HashMap Name String
buildOriginMap names modules =
    Map.fromList [(n, modKey)
        | n <- names
        , (modKey, menv) <- Map.toList modules
        , Set.member n (publicNames menv) || Set.member n (privateNames menv)
        ]
