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

import Data.List (nub)

import Surface
import State
import qualified ElabMetadata as EM
import ElabMetadata (binderVisibility, binderRole, defaultExplicitBinderInfo, isSemanticRole)
import ElabCore
import ElabObligation
import Monomorphize (monomorphizeLambdas, buildHandlerOpsMap)
import Specialize (specializeLambdas)

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
    , cpUnresolved   :: [Name]              -- ^ Unresolved implicit dispatch after mono (should be empty)
    } deriving (Show)

-- | Build a compilation plan for the given entry points and target.
-- This is the main entry point for any backend.
buildCompilationPlan :: Bool                -- ^ Debug tracing enabled
                     -> [Name]             -- ^ Entry point function names
                     -> Name               -- ^ Target name (e.g. "native")
                     -> MonomorphLevel     -- ^ How aggressively to resolve instances
                     -> SpecLevel          -- ^ How aggressively to specialize generic instances
                     -> Environment        -- ^ Full loaded environment
                     -> InterpreterState   -- ^ For module provenance
                     -> CompilationPlan
buildCompilationPlan debug entryPoints targetName monoLevel specLvl env state =
    let -- Phase A: Reachability analysis (pre-monomorphization)
        -- Pass target name so reachability prefers target instances over interpreter instances
        (reachFuncs, reachInsts, reachCons, reachTypes) =
            computeReachableSet entryPoints targetName monoLevel env

        -- Project reachable raw keys into the elaborated view explicitly.
        -- Reachability is computed over canonical/raw names, while mono/spec work
        -- best on elaborated lambdas when available.
        reachableLams = Map.fromList
            [ (k, lam)
            | k <- HSet.toList reachFuncs
            , Just lam <- [lookupLambdaRep ElabLambdaRep k env]
            ]
        reachableInsts = Map.fromList
            [ (k, lam)
            | k <- HSet.toList reachInsts
            , Just lam <- [lookupInstanceLambdaByKeyRep ElabLambdaRep k env]
            ]

        -- Phase A.2: Monomorphize only reachable functions, using FULL env for lookups.
        -- The full env is needed so lookupLambda can find dispatch templates like "+"
        (monoLams, monoInsts) = monomorphizeLambdas debug monoLevel targetName env reachableLams reachableInsts

        -- Phase A.3: Specialize generic instance functions for concrete type args.
        -- Creates specialized copies (e.g., show\0List\0$Int) and rewrites call sites.
        (specLams, specInsts) = case specLvl of
            SpecNone -> (monoLams, monoInsts)
            SpecFull -> specializeLambdas debug targetName env monoLams monoInsts

        -- Phase B: Collect extern refs from specialized bodies
        targetExts = case Map.lookup targetName (targetExterns env) of
            Just te -> te
            Nothing -> Map.empty
        externRefs = collectExternRefs specLams specInsts targetExts

        -- Module provenance
        allReachableNames = Map.keys reachableLams ++ Map.keys reachableInsts
        origin = buildOriginMap allReachableNames
                    (loadedModules (currentModuleEnv state))

        -- Phase C: Detect unresolved implicit dispatch after specialization
        -- After SpecFull, user functions and specialized instances (keys with $)
        -- should have zero unresolved calls. Generic instances may still have
        -- some if specialization couldn't resolve all type params.
        compiledFuncs = Map.filterWithKey (\n lam -> not (lambdaHasSemanticImplicit lam (lookupLambdaBinderInfo n env))) specLams
        specializedInsts = Map.filterWithKey (\k _ -> '$' `elem` k) specInsts
        checkSet = Map.union compiledFuncs specializedInsts
        unresolvedWithSource = [(fname ++ " (" ++ show (lamSrcInfo lam) ++ ")", u)
                               | (fname, lam) <- Map.toList checkSet
                               , u <- collectUnresolvedCalls targetName env lam]
        unresolved = nub [u ++ " in " ++ src | (src, u) <- unresolvedWithSource]

    in CompilationPlan
        { cpFunctions    = compiledFuncs
        , cpInstances    = specInsts
        , cpExternRefs   = externRefs
        , cpConstructors = reachCons
        , cpTypes        = reachTypes
        , cpEntryPoints  = entryPoints
        , cpModuleOrigin = origin
        , cpUnresolved   = unresolved
        }

-- | Compute the transitive closure of names reachable from entry points.
-- Uses target-specific instances when available (not all interpreter instances).
-- Returns (reachable functions, reachable instance keys, constructors, types).
computeReachableSet :: [Name] -> Name -> MonomorphLevel -> Environment
                    -> (HashSet Name, HashSet Name, HashSet Name, HashSet Name)
computeReachableSet entryPoints targetName monoLevel env =
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
                    -- Mark as visited to prevent re-processing (critical for MonoFull
                    -- where instance body refs can reference back to implicit-param functions)
                    let funcs' = HSet.insert name funcs
                    -- Check handler ops first (effectSeq, putStrLn, etc.)
                        handlerBodyRefs = case Map.lookup name handlerOpsMap of
                            Just hLam -> collectSurfaceRefs env (body hLam)
                            Nothing -> []
                    -- Then check target instances (algebra methods like +, show)
                        targetInstKeys = findInstanceKeysForFunc name tInstMap
                        targetBodyRefs = concatMap
                            (\k -> case Map.lookup k tInstMap of
                                Just lam -> collectSurfaceRefs env (body lam)
                                Nothing -> [])
                            targetInstKeys
                    -- When MonoFull, also add instance keys (so they get compiled)
                    -- and follow their bodies for transitive refs
                        instanceKeysAndRefs = if monoLevel == MonoFull
                            then let instKeys = findInstanceKeysForFunc name (instanceLambdas env)
                                     nonIntrinsicKeys = filter (\k -> case Map.lookup k (instanceLambdas env) of
                                        Just il -> body il /= Intrinsic
                                        _ -> False) instKeys
                                     bodyRefs = concatMap (\k -> case Map.lookup k (instanceLambdas env) of
                                        Just il | body il /= Intrinsic -> collectSurfaceRefs env (body il)
                                        _ -> []) instKeys
                                 in nonIntrinsicKeys ++ bodyRefs
                            else []
                        newWork = filter (notVisited funcs' insts)
                                    (handlerBodyRefs ++ targetBodyRefs ++ instanceKeysAndRefs)
                    in go funcs' insts cons typs (newWork ++ rest)

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
    go (Instance _ _ _ impls _) = concatMap go impls
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

-- | Collect names of dispatch calls that remain unresolved after monomorphization /
-- specialization. This must be target-aware: some dispatch heads (like effectSeq)
-- are provided by target handlers rather than top-level implicit functions.
collectUnresolvedCalls :: Name -> Environment -> Lambda -> [Name]
collectUnresolvedCalls targetName env lam =
    case Map.lookup (lamName lam) (topLambdasR env) of
        Just rlam -> collectUnresolvedCallsR targetName env rlam
        Nothing -> goSurface (body lam)
  where
    handlerOps = buildHandlerOpsMap targetName env

    isUnresolvedDispatchHead n =
        case lookupLambdaRep ElabLambdaRep n env of
            Just fun ->
                let infos = lookupAnyLambdaBinderInfo n env
                    hasSemanticImplicit = or
                        [ binderVisibility bi == EM.Implicit && isSemanticRole (binderRole bi)
                        | bi <- take (length (params fun)) (infos ++ repeat defaultExplicitBinderInfo)
                        ]
                in hasSemanticImplicit || Map.member n handlerOps
            Nothing -> Map.member n handlerOps

    goSurface (App (Id n) args) =
        let self = if isUnresolvedDispatchHead n then [n] else []
        in self ++ concatMap goSurface args
    goSurface (App f args) = goSurface f ++ concatMap goSurface args
    goSurface (Function inner) = goSurface (body inner)
    goSurface (LetIn binds bodyExpr) = concatMap (goSurface . snd) binds ++ goSurface bodyExpr
    goSurface (ExpandedCase checks bodyExpr _) = concatMap goPC checks ++ goSurface bodyExpr
    goSurface (PatternMatches cases) = concatMap goSurface cases
    goSurface (CaseOf cv ex _) = concatMap (goSurface . val) cv ++ goSurface ex
    goSurface (ConTuple _ args) = concatMap goSurface args
    goSurface (NTuple fields) = concatMap (goSurface . snd) fields
    goSurface (IfThenElse c t e) = goSurface c ++ goSurface t ++ goSurface e
    goSurface (BinaryOp _ l r) = goSurface l ++ goSurface r
    goSurface (ActionBlock stmts) = concatMap goAS stmts
    goSurface (RecFieldAccess _ e) = goSurface e
    goSurface (ArrayLit es) = concatMap goSurface es
    goSurface (Typed e _) = goSurface e
    goSurface (ReprCast e _) = goSurface e
    goSurface _ = []

    goPC (PatternGuard _ e) = goSurface e
    goPC _ = []

    goAS (ActionExpr e) = goSurface e
    goAS (ActionBind _ e) = goSurface e
    goAS (ActionLet _ e) = goSurface e

collectUnresolvedCallsR :: Name -> Environment -> ElabLambda -> [Name]
collectUnresolvedCallsR targetName env lam = go (elabLamBody lam)
  where
    handlerOps = buildHandlerOpsMap targetName env
    go (EVar _) = []
    go (EGlobal _) = []
    go (ELit _) = []
    go (EType _) = []
    go (EAnn e _) = go e
    go (ELam _ body _) = go body
    go (ELet binds bodyExpr) = concatMap (go . snd) binds ++ go bodyExpr
    go (ECall kind headExpr args _) =
        let self = case kind of
                SemanticCall obl -> [oblMethod obl]
                HandlerCall obl -> if Map.member (oblMethod obl) handlerOps then [] else [oblMethod obl]
                EvidenceCall _ -> []
                DirectCall -> []
                IntrinsicCall _ -> []
        in self ++ go headExpr ++ concatMap (go . elabArgExpr) args
    go (ESurface e) = goSurface e

    goSurface (App f args) = goSurface f ++ concatMap goSurface args
    goSurface (Function inner) = goSurface (body inner)
    goSurface (LetIn binds bodyExpr) = concatMap (goSurface . snd) binds ++ goSurface bodyExpr
    goSurface (ExpandedCase checks bodyExpr _) = concatMap goPC checks ++ goSurface bodyExpr
    goSurface (PatternMatches cases) = concatMap goSurface cases
    goSurface (CaseOf cv ex _) = concatMap (goSurface . val) cv ++ goSurface ex
    goSurface (ConTuple _ args) = concatMap goSurface args
    goSurface (NTuple fields) = concatMap (goSurface . snd) fields
    goSurface (IfThenElse c t e) = goSurface c ++ goSurface t ++ goSurface e
    goSurface (BinaryOp _ l r) = goSurface l ++ goSurface r
    goSurface (ActionBlock stmts) = concatMap goAS stmts
    goSurface (RecFieldAccess _ e) = goSurface e
    goSurface (ArrayLit es) = concatMap goSurface es
    goSurface (Typed e _) = goSurface e
    goSurface (ReprCast e _) = goSurface e
    goSurface _ = []

    goPC (PatternGuard _ e) = goSurface e
    goPC _ = []

    goAS (ActionExpr e) = goSurface e
    goAS (ActionBind _ e) = goSurface e
    goAS (ActionLet _ e) = goSurface e
