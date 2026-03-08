{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}

-- This is our PRIMARY CORE LANGUAGE, "Core List Machine",
-- where we use n-lists for tuples explicitly instead of lambda-applications.
-- NO TYPES, as they must be typechecked by this point fully.

module CLM
where

import Util.PrettyPrinting
import Logs
import Surface
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Data.Binary (Binary)

type CLMVar = (Name, CLMExpr) 

-- simple lambda:
-- lambda with cases:
-- Expr on the right side may ONLY be CLMCASE!!!
data CLMLam = CLMLam [CLMVar] CLMExpr | CLMLamCases [CLMVar] [CLMExpr]
    deriving (Show, Eq, Generic)
instance Binary CLMLam

type CLMConsTagCheck = (ConsTag, CLMExpr) -- legacy alias, kept for compatibility

data CLMPatternCheck = CLMCheckTag ConsTag CLMExpr | CLMCheckLit Literal CLMExpr
    deriving (Show, Eq, Generic)
instance Binary CLMPatternCheck

data CLMExpr = 
    CLMEMPTY
  | CLMERR String SourceInfo
  | CLMID Name
  | CLMLAM CLMLam
  | CLMBIND Name CLMExpr
  | CLMAPP CLMExpr [CLMExpr] -- direct function application (no dispatch needed)
  | CLMPAP CLMExpr [CLMExpr] -- partial application (type-known, no dispatch)
  | CLMCON ConsTag [CLMExpr] -- saturated constructor application (value)
  | CLMIAP CLMExpr [CLMExpr] -- implicit-param application: dispatches at runtime via
  -- type inference on args → intrinsic/instance lookup. Used for structure functions.
  | CLMFieldAccess (Name, Int) CLMExpr -- accessing a field of an expr by name or number
  | CLMCASE [CLMPatternCheck] CLMExpr -- list of pattern checks that must all fold to True bound to an expr
  | CLMPROG [CLMExpr] -- list of expressions, for now used for Action but needs to change
  | CLMTYPED CLMExpr CLMExpr -- in case we want to give a type to an expression
  | CLMPRIMCALL -- body of the function that is a primitive call
  | CLMLIT Literal
  | CLMU Level -- Universe hierarchy: CLMU (LConst 0) = Type, CLMU (LConst 1) = Kind, etc.
  | CLMARRAY [CLMExpr] -- array of CLM expressions
  | CLMREF Int -- opaque mutable reference handle (backed by IORef at runtime)
  | CLMMUTARRAY Int -- opaque mutable array handle (backed by IORef [CLMExpr] at runtime)
  -- Class system nodes
  | CLMMCALL CLMExpr Name [CLMExpr]  -- object.method(args) — dynamic dispatch
  | CLMSCALL CLMExpr Name [CLMExpr]  -- super.method(args) — parent dispatch
  | CLMNEW Name [CLMExpr]            -- ClassName.new(args) — construction
  | CLMHANDLE CLMExpr Name [(Name, CLMExpr)] [(Name, CLMExpr)]
    -- CLMHANDLE body effectName [(letName, letExpr)] [(opName, implementation)]
    -- Pushes handler ops onto dynamic handler stack, evals body, pops
    deriving (Show, Eq, Generic)

instance Binary CLMExpr

-- helper function that goes inside all pattern checks and
-- checks if they all evaluate to true
evalPatternChecks :: [CLMPatternCheck] -> Bool
evalPatternChecks [] = True
evalPatternChecks (ct:cts) = if runPatternCheck ct then evalPatternChecks cts else False

runPatternCheck :: CLMPatternCheck -> Bool
runPatternCheck (CLMCheckTag (ConsTag _ i) (CLMCON (ConsTag _ i1) _)) = (i == i1)
runPatternCheck (CLMCheckLit lit (CLMLIT lit2)) = (lit == lit2)
-- Handle field access in pattern checks: evaluate the access inline
runPatternCheck (CLMCheckTag ct (CLMFieldAccess ("", n) (CLMCON tag tuple)))
    | n < length tuple = runPatternCheck (CLMCheckTag ct (tuple !! n))
runPatternCheck (CLMCheckLit lit (CLMFieldAccess ("", n) (CLMCON _ tuple)))
    | n < length tuple = runPatternCheck (CLMCheckLit lit (tuple !! n))
runPatternCheck (CLMCheckTag ct (CLMFieldAccess (nm, n) (CLMCON tag tuple)))
    | n >= 0 && n < length tuple = runPatternCheck (CLMCheckTag ct (tuple !! n))
runPatternCheck (CLMCheckLit lit (CLMFieldAccess (nm, n) (CLMCON _ tuple)))
    | n >= 0 && n < length tuple = runPatternCheck (CLMCheckLit lit (tuple !! n))
runPatternCheck _ = False


resolveCase :: CLMExpr -> Maybe CLMExpr
resolveCase (CLMCASE cts ex) = if evalPatternChecks cts then Just ex else Nothing
resolveCase e = Nothing

-- this goes through all the cases in a function body and finds the first
-- one that evaluates fully
resolveCases :: CLMLam -> Either String CLMExpr
resolveCases lam@(CLMLamCases vars []) = Left $ "ERROR: No case resolved to an expression in " ++ ppr lam
resolveCases (CLMLamCases vars ((cs@(CLMCASE ctc ex)):cases) ) = 
    case resolveCase cs of
        Nothing -> resolveCases (CLMLamCases vars cases)
        Just e  -> Right e
resolveCases l = Left $ "ERROR: Unexpected expression among the cases in a function " ++ ppr l

-- this function applies a lambda to an array of arguments
-- full application returns body with substituted elements
-- partial application returns new function with new vars and partially substituted body
-- if there are more arguments than vars - return the body applied to the remaining vars
-- do we allow it???
applyCLMLam :: CLMLam -> [CLMExpr] -> CLMExpr
-- CLMLam: use simultaneous beta reduction (single tree walk for all params)
applyCLMLam (CLMLam vars body) args
    | nv == 0, na == 0 = body
    | nv > 0,  na == 0 = CLMLAM (CLMLam vars body)
    | nv == 0           = CLMAPP body args
    | nv == na          = simultBetaReduce (mkSubst vars args) body
    | nv > na           = -- partial application
        let (bound, free) = splitAt na vars
        in CLMLAM (CLMLam free (simultBetaReduce (mkSubst bound args) body))
    | otherwise         = -- over-application
        let (used, extra) = splitAt nv args
        in CLMAPP (simultBetaReduce (mkSubst vars used) body) extra
  where nv = length vars; na = length args
-- CLMLamCases: same approach but for case-based lambdas
applyCLMLam l@(CLMLamCases [] bodies) [] =
    case (resolveCases l) of
        Left err -> CLMERR err SourceInteractive
        Right ex -> ex
applyCLMLam (CLMLamCases vars bodies) args
    | nv > 0,  na == 0 = CLMLAM (CLMLamCases vars bodies)
    | nv == 0           = CLMAPP (CLMLAM (CLMLamCases [] bodies)) args
    | nv == na          = -- full application: substitute all at once
        let subst = mkSubst vars args
            bodies' = map (simultBetaReduce subst) bodies
        in case resolveCases (CLMLamCases [] bodies') of
            Left err -> CLMERR err SourceInteractive
            Right ex -> ex
    | nv > na           = -- partial: substitute what we can
        let (bound, free) = splitAt na vars
            subst = mkSubst bound args
        in CLMLAM (CLMLamCases free (map (simultBetaReduce subst) bodies))
    | otherwise         = -- over-application
        let (used, extra) = splitAt nv args
            subst = mkSubst vars used
            bodies' = map (simultBetaReduce subst) bodies
        in CLMAPP (CLMLAM (CLMLamCases [] bodies')) extra
  where nv = length vars; na = length args

-- Build substitution map from vars and args
mkSubst :: [CLMVar] -> [CLMExpr] -> Map.HashMap Name CLMExpr
mkSubst vars args = Map.fromList (zip (map fst vars) args)

-- | Simultaneous multi-variable beta reduction in a single tree walk.
-- Replaces all variable occurrences from the substitution map at once,
-- respecting variable shadowing in nested lambdas.
simultBetaReduce :: Map.HashMap Name CLMExpr -> CLMExpr -> CLMExpr
simultBetaReduce subst
    | Map.null subst = id  -- nothing to substitute
    | otherwise = go
  where
    go (CLMID name) = case Map.lookup name subst of
        Just val -> val
        Nothing  -> CLMID name
    go (CLMLAM (CLMLam args ex)) =
        let subst' = removeShadowed args
        in if Map.null subst' then CLMLAM (CLMLam args ex)
           else CLMLAM (CLMLam args (simultBetaReduce subst' ex))
    go (CLMLAM (CLMLamCases args exs)) =
        let subst' = removeShadowed args
        in if Map.null subst' then CLMLAM (CLMLamCases args exs)
           else CLMLAM (CLMLamCases args (map (simultBetaReduce subst') exs))
    go (CLMBIND n ex) = CLMBIND n (go ex)
    go (CLMAPP ex exs) = CLMAPP (go ex) (map go exs)
    go (CLMIAP ex exs) = CLMIAP (go ex) (map go exs)
    go (CLMPAP ex exs) = CLMPAP (go ex) (map go exs)
    go (CLMCON ct exs) = CLMCON ct (map go exs)
    go (CLMFieldAccess ac ex) = CLMFieldAccess ac (go ex)
    go (CLMCASE cts ex) = CLMCASE (map goCheck cts) (go ex)
    go (CLMPROG exs) = CLMPROG (map go exs)
    go (CLMTYPED ex1 ex2) = CLMTYPED (go ex1) (go ex2)
    go (CLMARRAY exs) = CLMARRAY (map go exs)
    go (CLMMCALL obj meth args) = CLMMCALL (go obj) meth (map go args)
    go (CLMSCALL obj meth args) = CLMSCALL (go obj) meth (map go args)
    go (CLMNEW nm args) = CLMNEW nm (map go args)
    go (CLMHANDLE bdy eff lets ops) = CLMHANDLE (go bdy) eff (map (\(n,v) -> (n, go v)) lets) (map (\(n,impl) -> (n, go impl)) ops)
    go e = e  -- CLMLIT, CLMEMPTY, CLMPRIMCALL, CLMU, CLMERR, CLMREF, CLMMUTARRAY
    goCheck (CLMCheckTag ct e) = CLMCheckTag ct (go e)
    goCheck (CLMCheckLit lit e) = CLMCheckLit lit (go e)
    -- Remove shadowed variables from substitution map
    removeShadowed args =
        let shadowedNames = Set.fromList (map fst args)
        in Map.filterWithKey (\k _ -> not (Set.member k shadowedNames)) subst

-- f(x) = expr, x = val, substituting all x appearances in expr for val
betaReduceCLM :: CLMVar -> CLMExpr -> CLMExpr
-- substituting all nm occurences in expr for val
-- no typechecking whatsoever
-- IMPORTANT: respects variable shadowing — does NOT substitute into lambda
-- bodies where a parameter shadows the variable being substituted
betaReduceCLM (nm,val) expr = go expr
  where go (CLMID name) = if nm == name then val else CLMID name
        go (CLMLAM (CLMLam args ex))
            | any (\(n,_) -> n == nm) args = CLMLAM (CLMLam args ex) -- shadowed, stop
            | otherwise = CLMLAM (CLMLam args (go ex))
        go (CLMLAM (CLMLamCases args exs))
            | any (\(n,_) -> n == nm) args = CLMLAM (CLMLamCases args exs) -- shadowed, stop
            | otherwise = CLMLAM (CLMLamCases args (map go exs))
        go (CLMBIND n ex) = CLMBIND n (go ex)
        go (CLMAPP ex exs) = CLMAPP (go ex) (map go exs)
        go (CLMIAP ex exs) = CLMIAP (go ex) (map go exs)
        go (CLMPAP ex exs) = CLMPAP (go ex) (map go exs)
        go (CLMCON ct exs) = CLMCON ct (map go exs)
        go (CLMFieldAccess ac ex) = CLMFieldAccess ac (go ex)
        go (CLMCASE cts ex) = CLMCASE (map goCheck cts) (go ex)
        go (CLMPROG exs) = CLMPROG (map go exs)
        go (CLMTYPED ex1 ex2) = CLMTYPED (go ex1) (go ex2)
        go (CLMARRAY exs) = CLMARRAY (map go exs)
        go (CLMMCALL obj meth args) = CLMMCALL (go obj) meth (map go args)
        go (CLMSCALL obj meth args) = CLMSCALL (go obj) meth (map go args)
        go (CLMNEW nm args) = CLMNEW nm (map go args)
        go (CLMHANDLE bdy eff lets ops) = CLMHANDLE (go bdy) eff (map (\(n,v) -> (n, go v)) lets) (map (\(n,impl) -> (n, go impl)) ops)
        go e = e
        goCheck (CLMCheckTag ct e) = CLMCheckTag ct (go e)
        goCheck (CLMCheckLit lit e) = CLMCheckLit lit (go e)

-- (map f (map (traverseCLMExpr f) exs) )
-- (f $ traverseCLMExpr f ex)
-- we DO NOT traverse left parts of lambda arguments
traverseCLMExpr :: (CLMExpr -> CLMExpr) -> CLMExpr -> CLMExpr
traverseCLMExpr f (CLMLAM (CLMLam args ex)) = (CLMLAM (CLMLam args (f $ traverseCLMExpr f ex)))
traverseCLMExpr f (CLMLAM (CLMLamCases arg exs)) = CLMLAM (CLMLamCases arg (map f (map (traverseCLMExpr f) exs) ))
traverseCLMExpr f (CLMBIND nm ex) = CLMBIND nm (f $ traverseCLMExpr f ex)
traverseCLMExpr f (CLMAPP ex exs) = CLMAPP (f $ traverseCLMExpr f ex) (map f (map (traverseCLMExpr f) exs) )
traverseCLMExpr f (CLMIAP ex exs) = CLMIAP (f $ traverseCLMExpr f ex) (map f (map (traverseCLMExpr f) exs) )
traverseCLMExpr f (CLMPAP ex exs) = CLMPAP (f $ traverseCLMExpr f ex) (map f (map (traverseCLMExpr f) exs) )
traverseCLMExpr f (CLMCON ct exs) = CLMCON ct (map f (map (traverseCLMExpr f) exs) )
traverseCLMExpr f (CLMFieldAccess ac ex) = CLMFieldAccess ac (f $ traverseCLMExpr f ex)
traverseCLMExpr f (CLMCASE cts ex) = CLMCASE (map traverseCheck cts) (f $ traverseCLMExpr f ex)
  where traverseCheck (CLMCheckTag ct e) = CLMCheckTag ct (f $ traverseCLMExpr f e)
        traverseCheck (CLMCheckLit lit e) = CLMCheckLit lit (f $ traverseCLMExpr f e)
traverseCLMExpr f (CLMPROG exs) = CLMPROG (map f (map (traverseCLMExpr f) exs) )
traverseCLMExpr f (CLMTYPED ex1 ex2) = CLMTYPED (f $ traverseCLMExpr f ex1) (f $ traverseCLMExpr f ex2)
traverseCLMExpr f (CLMARRAY exs) = CLMARRAY (map f (map (traverseCLMExpr f) exs))
traverseCLMExpr f (CLMMCALL obj meth args) = CLMMCALL (f $ traverseCLMExpr f obj) meth (map f (map (traverseCLMExpr f) args))
traverseCLMExpr f (CLMSCALL obj meth args) = CLMSCALL (f $ traverseCLMExpr f obj) meth (map f (map (traverseCLMExpr f) args))
traverseCLMExpr f (CLMNEW nm args) = CLMNEW nm (map f (map (traverseCLMExpr f) args))
traverseCLMExpr f (CLMHANDLE bdy eff lets ops) = CLMHANDLE (f $ traverseCLMExpr f bdy) eff (map (\(n,v) -> (n, f $ traverseCLMExpr f v)) lets) (map (\(n,impl) -> (n, f $ traverseCLMExpr f impl)) ops)
traverseCLMExpr f e = f e

-- | Truncate ppr output to N chars, appending "..." if truncated
pprSummary :: Int -> CLMExpr -> String
pprSummary n e =
    let s = ppr e
    in if length s <= n then s else Prelude.take n s ++ "..."

instance PrettyPrint CLMExpr where
    ppr (CLMERR err si) = (as [bold,red] "ERROR: ") ++ err ++ case si of { SourceInteractive -> ""; _ -> " " ++ show si }
    ppr (CLMID nm) = nm
    ppr (CLMLAM lam) = ppr lam
    ppr (CLMCASE cschecks ex) = showListWFormat ppr "{" "}" " && " "{}" cschecks ++ " -> " ++ ppr ex
    ppr (CLMCON (ConsTag nm i) exs) = as [bold,red] nm ++ " "
        ++ showListCuBr ppr exs
    ppr (CLMAPP ex exs) = as [bold] (ppr ex) ++ " " 
        ++ showListRoBr ppr exs
    ppr (CLMIAP ex exs) = as [bold,yellow] (ppr ex) ++ " [?] " 
        ++ showListRoBr ppr exs
    ppr (CLMFieldAccess ("", i) ex) = ppr ex ++ "." ++ show i
    ppr (CLMFieldAccess (nm, _) ex) = ppr ex ++ "." ++ nm
    ppr (CLMPROG exs) = showListWFormat ppr "{\n" "\n}" ",\n" "{}" exs
    ppr (CLMBIND nm ex) = (as [bold] nm) ++ " = " ++ (ppr ex)
    ppr (CLMU (LConst 0)) = "Type"
    ppr (CLMU (LConst n)) = "Type" ++ show n
    ppr (CLMU l) = "U(" ++ showLevel l ++ ")"
    ppr (CLMLIT (LInt n)) = show n
    ppr (CLMLIT (LFloat f)) = show f
    ppr (CLMLIT (LString s)) = show s
    ppr (CLMLIT (LChar c)) = show c
    ppr (CLMLIT (LList exs)) = "[" ++ showListPlainSep ppr ", " exs ++ "]"
    ppr (CLMLIT (LInt8 n)) = show n ++ "i8"
    ppr (CLMLIT (LInt16 n)) = show n ++ "i16"
    ppr (CLMLIT (LInt32 n)) = show n ++ "i32"
    ppr (CLMLIT (LInt64 n)) = show n ++ "i64"
    ppr (CLMLIT (LWord8 n)) = show n ++ "u8"
    ppr (CLMLIT (LWord16 n)) = show n ++ "u16"
    ppr (CLMLIT (LWord32 n)) = show n ++ "u32"
    ppr (CLMLIT (LWord64 n)) = show n ++ "u64"
    ppr (CLMLIT (LFloat32 f)) = show f ++ "f32"
    ppr (CLMLIT l) = show l
    ppr (CLMARRAY exs) = "[" ++ showListPlainSep ppr ", " exs ++ "]"
    ppr (CLMREF n) = "<ref:" ++ show n ++ ">"
    ppr (CLMMUTARRAY n) = "<mutarray:" ++ show n ++ ">"
    ppr (CLMMCALL obj meth args) = ppr obj ++ "." ++ as [bold,cyan] meth ++ showListRoBr ppr args
    ppr (CLMSCALL obj meth args) = as [bold,cyan] "super" ++ "." ++ meth ++ showListRoBr ppr args
    ppr (CLMNEW nm args) = as [bold,cyan] nm ++ ".new" ++ showListRoBr ppr args
    ppr (CLMHANDLE bdy eff lets ops) = "handle " ++ ppr bdy ++ " with " ++ eff ++ "{" ++ showListPlainSep (\(n,v) -> "let " ++ n ++ "=" ++ ppr v) ", " lets ++ (if null lets then "" else ", ") ++ showListPlainSep (\(n,impl) -> n ++ "=" ++ ppr impl) ", " ops ++ "}"
    ppr e = show e

instance PrettyPrint CLMConsTagCheck where
    ppr (ConsTag nm i, e) = ppr e ++ (as [bold,yellow] " cons is ") ++ nm ++ "(" ++ show i ++ ")"

instance PrettyPrint CLMPatternCheck where
    ppr (CLMCheckTag (ConsTag nm i) e) = ppr e ++ (as [bold,yellow] " cons is ") ++ nm ++ "(" ++ show i ++ ")"
    ppr (CLMCheckLit lit e) = ppr e ++ (as [bold,yellow] " == ") ++ show lit

pprVar1 (nm,ex) = nm

instance PrettyPrint CLMVar where
    ppr (nm,ex) = nm

instance PrettyPrint CLMLam where
    ppr (CLMLam args ex) = "λ " ++ (showListRoBr ppr args) ++ ". " ++ ppr ex
    ppr (CLMLamCases args exs) = "λ " ++ (showListRoBr ppr args) ++ ". "
        ++ (showListWFormat ppr "{\n" "\n}" ",\n" "{}" exs)

-- | Pure CLM evaluator for type-level normalization.
-- Reuses applyCLMLam/resolveCases — no code duplication with the runtime evaluator.
-- Handles: function application, pattern matching, constructor evaluation, field access.
-- Does NOT handle: IO intrinsics, mutable refs, effect handlers, CLMIAP dispatch.
-- The lookupFn parameter abstracts over where function definitions come from
-- (topLambdas during type checking, clmLambdas at runtime).
evalCLMPure :: (Name -> Maybe CLMLam) -> Int -> CLMExpr -> CLMExpr
evalCLMPure lookupFn = go
  where
    maxDepth = 1000
    go d _ | d > maxDepth = CLMERR "type-level evaluation depth exceeded" SourceInteractive
    -- Direct application to lambda value
    go d (CLMAPP (CLMLAM lam) args) =
        let args' = Prelude.map (go (d+1)) args
        in go (d+1) (applyCLMLam lam args')
    -- Named function application: look up and apply
    go d (CLMAPP (CLMID name) args) =
        let args' = Prelude.map (go (d+1)) args
        in case lookupFn name of
            Just clmLam -> go (d+1) (applyCLMLam clmLam args')
            Nothing     -> CLMAPP (CLMID name) args'
    -- Nested application: evaluate the function part first
    go d (CLMAPP f args) =
        let f' = go (d+1) f
            args' = Prelude.map (go (d+1)) args
        in case f' of
            CLMLAM lam -> go (d+1) (applyCLMLam lam args')
            _          -> CLMAPP f' args'
    -- Constructor: evaluate fields
    go d (CLMCON ct fields) = CLMCON ct (Prelude.map (go (d+1)) fields)
    -- Field access on known constructor
    go d (CLMFieldAccess (_, i) inner) =
        case go (d+1) inner of
            CLMCON _ fs | i >= 0 && i < Prelude.length fs -> go (d+1) (fs !! i)
            inner' -> CLMFieldAccess ("", i) inner'
    -- Let binding: substitute value into body
    go d (CLMBIND nm ex) =
        let val = go (d+1) ex
        in CLMBIND nm val  -- can't reduce further without knowing the continuation
    -- Universe: normalize level
    go _ (CLMU l) = CLMU (normalizeLevel l)
    -- Everything else: already a value or can't reduce purely
    go _ e = e

-- | Normalize a level by evaluating concrete arithmetic
normalizeLevel :: Level -> Level
normalizeLevel (LConst n) = LConst n
normalizeLevel (LVar n) = LVar n
normalizeLevel (LSucc l) = levelSucc (normalizeLevel l)
normalizeLevel (LMax l1 l2) = levelMax (normalizeLevel l1) (normalizeLevel l2)

