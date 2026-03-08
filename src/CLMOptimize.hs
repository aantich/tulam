{-# LANGUAGE OverloadedStrings #-}

-- | CLM Core-to-Core optimization pass framework.
-- Each pass is a named, individually-toggleable transformation on CLM expressions.
-- Passes run after CLM conversion (Pass 4) and before interpretation/codegen.
module CLMOptimize
    ( runCLMOptPasses
    , CLMOptPass(..)
    , allCLMPasses
    -- Utilities
    , freeVarsCLM
    , clmSize
    , isPureCLM
    , isValueCLM
    ) where

import CLM
import Surface (Name, Literal(..), ConsTag(..))
import Logs (SourceInfo(..))
import State
import Intrinsics (lookupIntrinsic, boolToCLM)

import Data.HashMap.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State.Strict (get, put, modify)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)

--------------------------------------------------------------------------------
-- Pass framework
--------------------------------------------------------------------------------

-- | A named, self-contained optimization pass
data CLMOptPass = CLMOptPass
    { passName  :: String                              -- e.g. "constant-fold"
    , passDescr :: String                              -- human description
    , passRun   :: Environment -> CLMExpr -> CLMExpr   -- pure transformation
    }

-- | Master registry of all available passes (in execution order)
allCLMPasses :: [CLMOptPass]
allCLMPasses =
    [ etaReducePass
    , inlineSmallPass
    , constantFoldPass
    , knownConstructorPass
    , deadCodeElimPass
    ]

-- | Run all enabled CLM optimization passes over clmLambdas and clmInstances.
-- Snapshots raw CLM before optimization for comparison.
runCLMOptPasses :: IntState ()
runCLMOptPasses = do
    st <- get
    let env = currentEnvironment st
    let flags = currentFlags st
    let optFlags = optSettings flags
    -- Snapshot raw CLM for :clm-raw comparison
    let env1 = env { rawCLMLambdas = clmLambdas env, rawCLMInstances = clmInstances env }
    if not (optimizeEnabled optFlags)
        then put st { currentEnvironment = env1 }
        else do
            put st { currentEnvironment = env1 }
            -- Run each enabled pass
            mapM_ (\pass -> do
                if isPassEnabled (passName pass) optFlags
                    then applyPassToAll pass
                    else pure ()
                ) allCLMPasses

-- | Check if a specific pass is enabled
isPassEnabled :: String -> OptFlags -> Bool
isPassEnabled nm flags = case nm of
    "eta-reduce"        -> optEtaReduce flags
    "constant-fold"     -> optConstantFold flags
    "known-constructor" -> optKnownConstructor flags
    "dead-code-elim"    -> optDeadCodeElim flags
    "inline-small"      -> optInlineSmall flags
    _                   -> True  -- unknown passes enabled by default

-- | Apply a pure pass to all CLM lambdas and instances
applyPassToAll :: CLMOptPass -> IntState ()
applyPassToAll pass = do
    st <- get
    let env = currentEnvironment st
    let f = passRun pass env
    let env' = env
            { clmLambdas = Map.map (applyToLam f) (clmLambdas env)
            , clmInstances = Map.map (applyToLam f) (clmInstances env)
            }
    put st { currentEnvironment = env' }
  where
    applyToLam :: (CLMExpr -> CLMExpr) -> CLMLam -> CLMLam
    applyToLam f (CLMLam vars body) = CLMLam vars (f body)
    applyToLam f (CLMLamCases vars bodies) = CLMLamCases vars (Prelude.map f bodies)

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- | Compute the set of free variables in a CLM expression
freeVarsCLM :: CLMExpr -> Set.Set Name
freeVarsCLM (CLMID nm)     = Set.singleton nm
freeVarsCLM (CLMLIT _)     = Set.empty
freeVarsCLM CLMEMPTY        = Set.empty
freeVarsCLM CLMPRIMCALL     = Set.empty
freeVarsCLM (CLMU _)        = Set.empty
freeVarsCLM (CLMERR _ _)    = Set.empty
freeVarsCLM (CLMREF _)      = Set.empty
freeVarsCLM (CLMMUTARRAY _) = Set.empty
freeVarsCLM (CLMMCALL obj _ args) = Set.union (freeVarsCLM obj) (Set.unions (Prelude.map freeVarsCLM args))
freeVarsCLM (CLMSCALL obj _ args) = Set.union (freeVarsCLM obj) (Set.unions (Prelude.map freeVarsCLM args))
freeVarsCLM (CLMNEW _ args)       = Set.unions (Prelude.map freeVarsCLM args)
freeVarsCLM (CLMCON _ exs)  = Set.unions (Prelude.map freeVarsCLM exs)
freeVarsCLM (CLMARRAY exs)  = Set.unions (Prelude.map freeVarsCLM exs)
freeVarsCLM (CLMAPP f exs)  = Set.union (freeVarsCLM f) (Set.unions (Prelude.map freeVarsCLM exs))
freeVarsCLM (CLMIAP f exs)  = Set.union (freeVarsCLM f) (Set.unions (Prelude.map freeVarsCLM exs))
freeVarsCLM (CLMPAP f exs)  = Set.union (freeVarsCLM f) (Set.unions (Prelude.map freeVarsCLM exs))
freeVarsCLM (CLMBIND nm ex) = Set.delete nm (freeVarsCLM ex)
freeVarsCLM (CLMFieldAccess _ ex) = freeVarsCLM ex
freeVarsCLM (CLMPROG exs)   = Set.unions (Prelude.map freeVarsCLM exs)
freeVarsCLM (CLMTYPED e1 e2) = Set.union (freeVarsCLM e1) (freeVarsCLM e2)
freeVarsCLM (CLMCASE checks body) =
    Set.union (Set.unions (Prelude.map freeVarsCheck checks)) (freeVarsCLM body)
  where
    freeVarsCheck (CLMCheckTag _ e) = freeVarsCLM e
    freeVarsCheck (CLMCheckLit _ e) = freeVarsCLM e
freeVarsCLM (CLMHANDLE bdy _ lets ops) = Set.unions [freeVarsCLM bdy, Set.unions (Prelude.map (freeVarsCLM . snd) lets), Set.unions (Prelude.map (freeVarsCLM . snd) ops)]
freeVarsCLM (CLMLAM lam) = freeVarsLam lam
  where
    freeVarsLam (CLMLam vars body) =
        Set.difference (freeVarsCLM body) (Set.fromList (Prelude.map fst vars))
    freeVarsLam (CLMLamCases vars bodies) =
        Set.difference (Set.unions (Prelude.map freeVarsCLM bodies)) (Set.fromList (Prelude.map fst vars))

-- | Count the number of AST nodes in a CLM expression
clmSize :: CLMExpr -> Int
clmSize (CLMID _)     = 1
clmSize (CLMLIT _)    = 1
clmSize CLMEMPTY       = 1
clmSize CLMPRIMCALL    = 1
clmSize (CLMU _)       = 1
clmSize (CLMERR _ _)   = 1
clmSize (CLMREF _)     = 1
clmSize (CLMMUTARRAY _) = 1
clmSize (CLMMCALL obj _ args) = 1 + clmSize obj + sum (Prelude.map clmSize args)
clmSize (CLMSCALL obj _ args) = 1 + clmSize obj + sum (Prelude.map clmSize args)
clmSize (CLMNEW _ args)       = 1 + sum (Prelude.map clmSize args)
clmSize (CLMCON _ exs) = 1 + sum (Prelude.map clmSize exs)
clmSize (CLMARRAY exs) = 1 + sum (Prelude.map clmSize exs)
clmSize (CLMAPP f exs) = 1 + clmSize f + sum (Prelude.map clmSize exs)
clmSize (CLMIAP f exs) = 1 + clmSize f + sum (Prelude.map clmSize exs)
clmSize (CLMPAP f exs) = 1 + clmSize f + sum (Prelude.map clmSize exs)
clmSize (CLMBIND _ ex) = 1 + clmSize ex
clmSize (CLMFieldAccess _ ex) = 1 + clmSize ex
clmSize (CLMPROG exs)  = 1 + sum (Prelude.map clmSize exs)
clmSize (CLMTYPED e1 e2) = 1 + clmSize e1 + clmSize e2
clmSize (CLMCASE checks body) = 1 + sum (Prelude.map checkSize checks) + clmSize body
  where checkSize (CLMCheckTag _ e) = 1 + clmSize e
        checkSize (CLMCheckLit _ e) = 1 + clmSize e
clmSize (CLMLAM (CLMLam vars body)) = 1 + clmSize body
clmSize (CLMLAM (CLMLamCases vars bodies)) = 1 + sum (Prelude.map clmSize bodies)

-- | Check if a CLM expression is pure (no IO/effects)
isPureCLM :: CLMExpr -> Bool
isPureCLM (CLMIAP _ _)  = False  -- may dispatch to IO
isPureCLM (CLMPROG _)   = False  -- sequential effects
isPureCLM (CLMERR _ _)  = False  -- errors
isPureCLM (CLMMCALL _ _ _) = False  -- dynamic dispatch (may have side effects)
isPureCLM (CLMSCALL _ _ _) = False  -- super dispatch (may have side effects)
isPureCLM (CLMNEW _ _)  = False  -- construction (may have side effects)
isPureCLM (CLMHANDLE _ _ _ _) = False  -- effect handler (inherently effectful)
isPureCLM (CLMAPP f exs) = isPureCLM f && Prelude.all isPureCLM exs
isPureCLM (CLMCON _ exs) = Prelude.all isPureCLM exs
isPureCLM (CLMARRAY exs) = Prelude.all isPureCLM exs
isPureCLM (CLMCASE checks body) = isPureCLM body
isPureCLM (CLMLAM _)    = True  -- lambdas are values
isPureCLM (CLMLIT _)    = True
isPureCLM (CLMID _)     = True
isPureCLM CLMEMPTY       = True
isPureCLM (CLMU _)       = True
isPureCLM CLMPRIMCALL    = True
isPureCLM (CLMFieldAccess _ ex) = isPureCLM ex
isPureCLM (CLMTYPED e _) = isPureCLM e
isPureCLM (CLMBIND _ ex) = isPureCLM ex
isPureCLM (CLMPAP f exs) = isPureCLM f && Prelude.all isPureCLM exs
isPureCLM (CLMREF _)    = True   -- ref handle is a value
isPureCLM (CLMMUTARRAY _) = True -- mut array handle is a value

-- | Check if a CLM expression is a fully evaluated value
isValueCLM :: CLMExpr -> Bool
isValueCLM (CLMLIT _)    = True
isValueCLM (CLMCON _ exs) = Prelude.all isValueCLM exs
isValueCLM (CLMLAM _)    = True
isValueCLM (CLMARRAY exs) = Prelude.all isValueCLM exs
isValueCLM CLMEMPTY       = True
isValueCLM (CLMREF _)     = True
isValueCLM (CLMMUTARRAY _) = True
isValueCLM _              = False

--------------------------------------------------------------------------------
-- Safe recursive descent (unlike traverseCLMExpr, returns atoms unchanged)
--------------------------------------------------------------------------------

-- | Descend into CLM children, applying f to each child.
-- Unlike traverseCLMExpr, this does NOT apply f to atomic nodes (CLMID, CLMLIT, etc.)
-- which avoids infinite recursion when f itself calls descendCLM.
descendCLM :: (CLMExpr -> CLMExpr) -> CLMExpr -> CLMExpr
descendCLM f (CLMLAM (CLMLam args ex)) = CLMLAM (CLMLam args (f ex))
descendCLM f (CLMLAM (CLMLamCases args exs)) = CLMLAM (CLMLamCases args (Prelude.map f exs))
descendCLM f (CLMBIND nm ex) = CLMBIND nm (f ex)
descendCLM f (CLMAPP ex exs) = CLMAPP (f ex) (Prelude.map f exs)
descendCLM f (CLMIAP ex exs) = CLMIAP (f ex) (Prelude.map f exs)
descendCLM f (CLMPAP ex exs) = CLMPAP (f ex) (Prelude.map f exs)
descendCLM f (CLMCON ct exs) = CLMCON ct (Prelude.map f exs)
descendCLM f (CLMFieldAccess ac ex) = CLMFieldAccess ac (f ex)
descendCLM f (CLMCASE cts ex) = CLMCASE (Prelude.map goCheck cts) (f ex)
  where goCheck (CLMCheckTag ct e) = CLMCheckTag ct (f e)
        goCheck (CLMCheckLit lit e) = CLMCheckLit lit (f e)
descendCLM f (CLMPROG exs) = CLMPROG (Prelude.map f exs)
descendCLM f (CLMTYPED e1 e2) = CLMTYPED (f e1) (f e2)
descendCLM f (CLMARRAY exs) = CLMARRAY (Prelude.map f exs)
descendCLM f (CLMMCALL obj meth args) = CLMMCALL (f obj) meth (Prelude.map f args)
descendCLM f (CLMSCALL obj meth args) = CLMSCALL (f obj) meth (Prelude.map f args)
descendCLM f (CLMNEW nm args) = CLMNEW nm (Prelude.map f args)
descendCLM f (CLMHANDLE bdy eff lets ops) = CLMHANDLE (f bdy) eff (Prelude.map (\(n,v) -> (n, f v)) lets) (Prelude.map (\(n,impl) -> (n, f impl)) ops)
-- Atomic nodes: return unchanged
descendCLM _ e = e

--------------------------------------------------------------------------------
-- Pass 1: Eta Reduction
--------------------------------------------------------------------------------

etaReducePass :: CLMOptPass
etaReducePass = CLMOptPass
    { passName  = "eta-reduce"
    , passDescr = "Simplify \\x -> f(x) to f when x is not free in f"
    , passRun   = \_ -> etaReduce
    }

-- | Eta-reduce: \x -> f(x) => f, \x,y -> f(x,y) => f
-- Only when params appear exactly in order as the application args
-- and none are free in the function position.
etaReduce :: CLMExpr -> CLMExpr
etaReduce = go
  where
    go expr = case expr of
        -- Lambda wrapping a simple application
        CLMLAM (CLMLam vars body) | not (Prelude.null vars) ->
            case body of
                CLMAPP (CLMID f) args -> tryEta vars f args (CLMID f)
                -- Never eta-reduce CLMIAP: implicit-param dispatch needs all args present
                -- Recurse into body if no eta opportunity at this level
                _ -> CLMLAM (CLMLam vars (go body))
        -- Recurse into composite subexpressions (NOT using traverseCLMExpr to avoid infinite loop)
        _ -> descendCLM go expr

    tryEta vars f args origFn
        | Prelude.length vars == Prelude.length args
        , argsMatchParams vars args
        , not (Set.member f (Set.fromList (Prelude.map fst vars)))
        = origFn
        | otherwise = CLMLAM (CLMLam vars (go (CLMAPP origFn args)))

    argsMatchParams [] [] = True
    argsMatchParams ((nm,_):vs) (CLMID arg:as) = nm == arg && argsMatchParams vs as
    argsMatchParams _ _ = False

--------------------------------------------------------------------------------
-- Pass 2: Constant Folding
--------------------------------------------------------------------------------

constantFoldPass :: CLMOptPass
constantFoldPass = CLMOptPass
    { passName  = "constant-fold"
    , passDescr = "Evaluate intrinsic functions on literal arguments at compile time"
    , passRun   = \_ -> constantFold
    }

-- | Fold intrinsic calls on literals: (+)(3, 4) => 7
constantFold :: CLMExpr -> CLMExpr
constantFold = go
  where
    go expr = case expr of
        -- Don't fold inside type-directed dispatch annotations;
        -- the inner CLMIAP must survive to runtime for dispatch
        CLMTYPED inner hint -> CLMTYPED inner hint
        -- Binary intrinsic on two literals (CLMIAP dispatch)
        CLMIAP (CLMID funcNm) [CLMLIT a, CLMLIT b] ->
            tryFoldBinary funcNm a b (CLMIAP (CLMID funcNm) [CLMLIT a, CLMLIT b])
        -- Unary intrinsic on one literal (CLMIAP dispatch)
        CLMIAP (CLMID funcNm) [CLMLIT a] ->
            tryFoldUnary funcNm a (CLMIAP (CLMID funcNm) [CLMLIT a])
        -- Binary intrinsic on two literals (CLMAPP direct call)
        CLMAPP (CLMID funcNm) [CLMLIT a, CLMLIT b] ->
            tryFoldBinary funcNm a b (CLMAPP (CLMID funcNm) [CLMLIT a, CLMLIT b])
        -- Unary intrinsic on one literal (CLMAPP direct call)
        CLMAPP (CLMID funcNm) [CLMLIT a] ->
            tryFoldUnary funcNm a (CLMAPP (CLMID funcNm) [CLMLIT a])
        -- Recurse
        _ -> descendCLM go expr

    tryFoldBinary funcNm a b fallback =
        let typeName = literalTypeName a
        in case typeName of
            Just tn -> case lookupIntrinsic funcNm tn of
                Just intrFn -> case intrFn [CLMLIT a, CLMLIT b] of
                    Just result -> result
                    Nothing -> fallback
                Nothing -> fallback
            Nothing -> fallback

    tryFoldUnary funcNm a fallback =
        let typeName = literalTypeName a
        in case typeName of
            Just tn -> case lookupIntrinsic funcNm tn of
                Just intrFn -> case intrFn [CLMLIT a] of
                    Just result -> result
                    Nothing -> fallback
                Nothing -> fallback
            Nothing -> fallback

-- | Determine the type name for a literal (for intrinsic lookup)
literalTypeName :: Literal -> Maybe Name
literalTypeName (LInt _)     = Just "Int"
literalTypeName (LFloat _)   = Just "Float64"
literalTypeName (LString _)  = Just "String"
literalTypeName (LChar _)    = Just "Char"
literalTypeName (LInt8 _)    = Just "Int8"
literalTypeName (LInt16 _)   = Just "Int16"
literalTypeName (LInt32 _)   = Just "Int32"
literalTypeName (LInt64 _)   = Just "Int64"
literalTypeName (LWord8 _)   = Just "Word8"
literalTypeName (LWord16 _)  = Just "Word16"
literalTypeName (LWord32 _)  = Just "Word32"
literalTypeName (LWord64 _)  = Just "Word64"
literalTypeName (LFloat32 _) = Just "Float32"
literalTypeName _            = Nothing

--------------------------------------------------------------------------------
-- Pass 3: Known-Constructor Optimization
--------------------------------------------------------------------------------

knownConstructorPass :: CLMOptPass
knownConstructorPass = CLMOptPass
    { passName  = "known-constructor"
    , passDescr = "Resolve pattern matches on known constructors/literals at compile time"
    , passRun   = \_ -> knownConstructor
    }

-- | When CLMCASE checks a known CLMCON/CLMLIT scrutinee, resolve statically
knownConstructor :: CLMExpr -> CLMExpr
knownConstructor = go
  where
    go expr = case expr of
        -- Single case with all-known checks
        CLMCASE checks body
            | Prelude.null checks -> CLMCASE [] (go body)  -- always-match: preserve wrapper for CLMLamCases invariant
            | allChecksKnown checks ->
                if evalPatternChecks checks
                    then go body  -- match succeeded, continue optimizing body
                    else CLMEMPTY -- match failed, dead code
            | otherwise -> CLMCASE checks (go body)
        -- CLMLamCases: resolve known branches
        CLMLAM (CLMLamCases vars bodies) ->
            let bodies' = Prelude.map go bodies
            in CLMLAM (CLMLamCases vars bodies')
        -- Recurse
        _ -> descendCLM go expr

    -- Check if all scrutinees in pattern checks are known values
    allChecksKnown [] = True
    allChecksKnown (CLMCheckTag _ e : cs) = isKnownScrutinee e && allChecksKnown cs
    allChecksKnown (CLMCheckLit _ e : cs) = isKnownScrutinee e && allChecksKnown cs

    isKnownScrutinee (CLMCON _ _) = True
    isKnownScrutinee (CLMLIT _) = True
    -- Field access on known constructor
    isKnownScrutinee (CLMFieldAccess (_, n) (CLMCON _ tuple)) = n >= 0 && n < Prelude.length tuple
    isKnownScrutinee _ = False

--------------------------------------------------------------------------------
-- Pass 4: Dead Code Elimination
--------------------------------------------------------------------------------

deadCodeElimPass :: CLMOptPass
deadCodeElimPass = CLMOptPass
    { passName  = "dead-code-elim"
    , passDescr = "Remove unreachable case branches and simplify single-branch functions"
    , passRun   = \_ -> deadCodeElim
    }

-- | Remove dead case branches and simplify CLMLamCases with single remaining case
deadCodeElim :: CLMExpr -> CLMExpr
deadCodeElim = go
  where
    go expr = case expr of
        -- CLMLamCases: filter out dead branches
        CLMLAM (CLMLamCases vars bodies) ->
            let bodies' = Prelude.filter (not . isDeadCase) (Prelude.map go bodies)
            in case bodies' of
                []  -> CLMERR "All cases eliminated (dead code)" SourceInteractive
                [b] -> CLMLAM (CLMLam vars b)  -- single case: simplify to CLMLam
                bs  -> CLMLAM (CLMLamCases vars bs)
        -- Remove CLMEMPTY from case results (generated by known-constructor)
        _ -> descendCLM go expr

    -- A case is dead if it's CLMEMPTY (resolved-away by known-constructor)
    -- or if it's a CLMERR
    isDeadCase CLMEMPTY     = True
    isDeadCase (CLMERR _ _) = True
    isDeadCase _            = False

--------------------------------------------------------------------------------
-- Pass 5: Small Function Inlining
--------------------------------------------------------------------------------

inlineSmallPass :: CLMOptPass
inlineSmallPass = CLMOptPass
    { passName  = "inline-small"
    , passDescr = "Inline small non-recursive functions at call sites"
    , passRun   = inlineSmall
    }

-- | Inline small non-recursive CLMLam functions at CLMAPP call sites.
-- Only inlines direct calls (CLMAPP), never implicit-param dispatch (CLMIAP).
-- Size threshold: 15 AST nodes. Skips recursive functions.
inlineSmall :: Environment -> CLMExpr -> CLMExpr
inlineSmall env = go
  where
    lamMap = clmLambdas env
    go expr = case expr of
        CLMAPP (CLMID f) args ->
            case Map.lookup f lamMap of
                Just lam@(CLMLam vars body)
                    | clmSize body < 15
                    , not (Set.member f (freeVarsCLM body))  -- non-recursive
                    -> go (applyCLMLam lam (Prelude.map go args))  -- inline + recurse
                _ -> descendCLM go expr
        _ -> descendCLM go expr
