{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

-- | Pass 2: Case Optimization
-- Expands PatternMatches into ExpandedCase with flattened constructor tag
-- and literal checks. Handles nested constructor patterns via double-recursive
-- descent (caseTransformApp1/App2).
module CaseOptimization
    ( caseOptimizationPass
    , checkSealedExhaustiveness
    , positivityCheckPass
    , terminationCheckPass
    , coverageCheckPass
    ) where

import State
import Surface
import Logs

import Control.Monad (when)
import Control.Monad.Trans.State.Strict
import Util.PrettyPrinting as TC

import Data.HashMap.Strict as Map
import qualified Data.List
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- PASS 2: Preliminary Optimizations and basic sanity checks
-- now that we have built the environment (so top level lambda and types bidnings)
-- we can start the optimizations
-- This pass includes proper formation of the Case pattern matches inside
-- functions
--------------------------------------------------------------------------------
caseOptimizationPass :: IntState()
caseOptimizationPass = do
    s <- get
    let env = currentEnvironment s
    let lambdas = topLambdas env
    lambdas' <- traverseWithKey f lambdas
    -- also optimize instance lambdas
    let instLambdas = instanceLambdas env
    instLambdas' <- traverseWithKey f instLambdas
    put s{currentEnvironment = env {topLambdas = lambdas', instanceLambdas = instLambdas'} }
    verboseLog $ "  Pass 2: optimized " ++ show (Map.size lambdas') ++ " lambdas, "
        ++ show (Map.size instLambdas') ++ " instances"
    where f k lam@(Lambda nm args (PatternMatches exs) tp _ _) = do
                exs' <- mapM (expandCase lam) exs
                exs'' <- mapM expandNestedPM exs'
                return lam {body = PatternMatches exs''}
          f k lam = do
                body' <- expandNestedPM (body lam)
                return lam {body = body'}

-- | Traverse an expression looking for nested Function nodes with PatternMatches
-- bodies, and expand their CaseOf nodes. This handles the inline match pattern:
--   match expr | pat -> body
-- which desugars to App (Function (Lambda "__m" [] (PatternMatches [CaseOf ...]))) [expr]
-- The case optimization pass only processes top-level lambda bodies, missing these.
expandNestedPM :: Expr -> IntState Expr
expandNestedPM (Function lam@(Lambda _ _ (PatternMatches exs) _ _ _)) = do
    exs' <- mapM (expandCase lam) exs
    exs'' <- mapM expandNestedPM exs'
    return $ Function (lam {body = PatternMatches exs''})
expandNestedPM (Function lam) = do
    body' <- expandNestedPM (body lam)
    return $ Function (lam {body = body'})
expandNestedPM (App f args) = do
    f' <- expandNestedPM f
    args' <- mapM expandNestedPM args
    return $ App f' args'
expandNestedPM (PatternMatches exs) = do
    exs' <- mapM expandNestedPM exs
    return $ PatternMatches exs'
expandNestedPM (CaseOf vs ex si) = do
    ex' <- expandNestedPM ex
    return $ CaseOf vs ex' si
expandNestedPM (ExpandedCase checks ex si) = do
    ex' <- expandNestedPM ex
    return $ ExpandedCase checks ex' si
expandNestedPM (Binding v) = do
    val' <- expandNestedPM (val v)
    return $ Binding (v {val = val'})
expandNestedPM (Statements exs) = do
    exs' <- mapM expandNestedPM exs
    return $ Statements exs'
expandNestedPM (BinaryOp op l r) = do
    l' <- expandNestedPM l
    r' <- expandNestedPM r
    return $ BinaryOp op l' r'
expandNestedPM (NTuple fields) = do
    fields' <- mapM (\(n,e) -> do { e' <- expandNestedPM e; return (n, e') }) fields
    return $ NTuple fields'
expandNestedPM (ConTuple ct exs) = do
    exs' <- mapM expandNestedPM exs
    return $ ConTuple ct exs'
expandNestedPM (Typed e ty) = do
    e' <- expandNestedPM e
    return $ Typed e' ty
expandNestedPM e = return e  -- atoms and other leaf nodes

-- choose which function to run
localMaybeAlt :: Maybe a -> IntState b -> (a -> IntState b) -> IntState b
localMaybeAlt Nothing  f g = f
localMaybeAlt (Just x) f g = g x


maybeEither :: Maybe a -> b -> (a -> b) -> b
maybeEither Nothing  d f = d
maybeEither (Just x) d f = f x

-- this one checks for ids that maybe arg=0 constructor applications
-- and fixes the expression properly
fixEmptyConstructor :: Environment -> Expr -> Expr
fixEmptyConstructor env ex@(Id name) =
    let mcons = lookupConstructor name env in
    case mcons of
        Nothing -> ex
        Just (cons,i) -> ConTuple (ConsTag name i) []
fixEmptyConstructor env e = e

fixEmptyConstructors ex = do
    s <- get
    let env = currentEnvironment s
    pure $ traverseExpr (fixEmptyConstructor env) ex

-- this function is a mouthful and needs to be refactored A LOT
expandCase :: Lambda -> Expr -> IntState Expr
-- First, we expand nested Constructor applications in the pattern
-- matches into the flat list, left to right
-- Then, we map over all of the case x of val statements,
-- check if val has a top level binding and if yes - keep it,
-- if not - means it's a variable substitution and we need to do a
-- beta reduction
expandCase lam cs@(CaseOf recs ex si) = do
    -- liftIO $ putStrLn $ "Analyzing: " ++ ppr cs
    -- ex111 <- fixEmptyConstructors ex
    (cases, ex') <- (t recs ex ([]))
    return $ ExpandedCase cases ex' si
    where
        t [] expr cases = return (cases, expr)
        -- case nm of val
        t (v@(Var nm tp val):xs) expr cases = do
            -- liftIO $ putStrLn $ "Processing case of: " ++ ppVarCaseOf v
            -- liftIO $ putStrLn $ "Current expr is: " ++ ppr expr
            s <- get
            let env = currentEnvironment s
            case val of
                (Id name) -> do
                    res <- caseTransformIdTop env (Id nm) name expr
                    case res of
                        -- errors are terminating!
                        Left er -> do
                                    let lpl = mkLogPayload si (er ++ (ppr v) ++ "\n")
                                    logError lpl
                                    return (cases, expr)
                        Right (cases', expr') -> do t xs expr' (Prelude.concat[cases',cases])


                -- case nm of vval, first level constructor application:
                -- case x of Succ(n) -> App (Id Succ) [Id n]
                -- here we need to go deep and beta reduce
                -- n = tupleField(0,x)
                vval@(App (Id cons) ex5) -> do
                    -- (liftIO $ putStrLn $ "App case in t: \n")
                    -- liftIO $ pPrint vval

                    res1 <- caseTransformApp1 env (Id nm) cons ex5
                    case res1 of
                        -- error case is terminating!!!
                        Left er -> do
                                        let lpl = mkLogPayload si (er ++ (ppr v) ++ "\n")
                                        logError lpl
                                        return (cases, expr)
                        Right cs -> do
                            let newBoundVarExpr = (Id nm)
                            -- liftIO $ putStrLn $ "Created field access on top: " ++ ppr newBoundVarExpr
                            -- launching next level of recursion:
                            (cases',expr', errs') <- caseTransformApp2 0 False env newBoundVarExpr cons ex5 expr [] []

                            mapM_ (\er -> do
                                            let lpl = mkLogPayload si (er ++ (ppr v) ++ "\n")
                                            logError lpl)
                                errs'
                            t xs expr' (Prelude.concat[cases,cs,cases'])
                            -- t (i+1) xs expr' (Prelude.concat[cases,cases'])

                -- case nm of literal value
                (Lit lit) -> do
                    let checkCase = ExprLiteralCheck lit (Id nm)
                    t xs expr (cases ++ [checkCase])


expandCase lam e = pure e

-- Case conversion for the pattern match is a sort of "double-recursive"
-- function that does the following:
-- when top-level pattern match check finds a constructor application
-- Con (a1, a2, Con2(b1, b2...)) it takes its arguments, existing cases
-- array, and iterates from left to right doing:
-- 1) beta-reduce when encountering Id n
-- 2) when encountering App (so a new cons application) -
--    - add a new case analysis into that array
--    - call itself to go down the tree

-- In all of these functions we are working with 2 arguments:
-- 1) list of consTag checks or equality checks that must be
--    folded in order with logical "and" and produce True in order for
-- 2) the right hand expression to be executed

-- First, helper pure functions:
-- case boundVarName of Id name -- difference between top level
-- and next level is only in how we make the beta-reduce, so
-- this one gets passed different functions as the first argument and that's it
caseTransformId :: (Expr->Expr) -> Environment -> Expr -> Name -> Expr -> IntState (Either String ([Expr],Expr))
caseTransformId f env boundVarExpr name expr = do
    -- liftIO $ putStrLn $ "Inside caseTransformId: name = " ++ name ++ " boundVar = " ++ ppr boundVarExpr
    maybeEither (lookupConstructor name env)
            (let vt = Var name UNDEFINED (f boundVarExpr)
                 expr' = betaReduce vt expr
             in  do
                    -- liftIO $ putStrLn $ "after beta reduce: " ++ ppr expr'
                    return $ Right ([], expr'))
            -- ^^^ nothing found in the environment, making beta-reduce
            -- and returning NOTHING in place of the old case
            -- otherwise checking if it's a constructor and if it is,
            -- returning a correct new "case" expression
            (\(lambda, i) -> pure $ Right ([ExprConsTagCheck (ConsTag name i) (f boundVarExpr) ],expr))


-- for top level id, we are passing id as a function
caseTransformIdTop = caseTransformId id
-- for next level, we need to build a proper field access:
caseTransformIdInternal i = caseTransformId (mkTupleFieldAccessExpr i)
-- case boundVarName of Id name -- processing id inside constructor applications
-- example:
-- { Cons(a1,a2) } -> g(a1,a2)
-- once we are inside Cons we process a1 as tupleField(0,boundExpr)
-- where boundExpr in our case will be simply Id boundVarName
-- but as we go deeper it will build up as corresponding calls

-- now for the more complicated case of App ...
-- it is recursive in itself + requires some additional error checks
-- env: environment, boundVarName -
-- case boundVar name of (App (Id name) ex) -> expr
-- so name is a constructor name. We need to also do an error check
-- if it's not found in the environment!!!

-- so, this first function simply transforms the case statement to the
-- constructor check, plus checks for errors if there's no such constructor
-- in the environment. Thus we don't need to deal with the RHS of this case here.
caseTransformApp1 :: Environment -> Expr -> Name -> [Expr] -> IntState (Either String [Expr])
caseTransformApp1 env boundVarExpr name ex = do
    -- liftIO $ putStrLn $ "Inside caseTransformApp1m name: " ++ name
    -- liftIO $ putStrLn $ "Bound var: " ++ ppr boundVarExpr
    maybeEither (lookupConstructor name env)
            (return $ Left ("Error: constructor " ++ name ++ " is not found in the environment"))
            -- ^^^ nothing found in the environment, it's an ERROR!
            -- otherwise
            (\(lambda, i) ->
                if (arity lambda == Prelude.length ex)
                then return $ Right [ExprConsTagCheck (ConsTag name i) boundVarExpr ]
                else return $ Left ("Error: constructor " ++ name ++ " application expects " ++ show (arity lambda) ++ " arguments and was given " ++ show (Prelude.length ex)))


-- case boundVar name of (App (Id name) ex) -> exp)r
-- this one falls inside the "ex" (e.g., Con (a1,a2,...) )
-- and iterates through it while building
caseTransformApp2 :: Int -> Bool -> Environment -> Expr -> Name -> [Expr] -> Expr -> [Expr] -> [String] -> IntState ([Expr], Expr, [String])
caseTransformApp2 i isTop env boundVarExpr name []          expr cases errs = return (cases,expr, errs)
-- case with ids is pretty straightforward
caseTransformApp2 i isTop env boundVarExpr name ((Id x):xs) expr cases errs = do
    -- (liftIO $ putStrLn $ "Id case in caseTransformApp2: " ++ x ++ " name: " ++ name)
    -- (liftIO $ putStrLn $ "Bound var expr: " ++ ppr boundVarExpr)
    res <- if isTop then caseTransformIdTop env boundVarExpr x expr
           else caseTransformIdInternal i env boundVarExpr x expr
    case (res) of
            -- error case is terminating!!!
            Left er -> return (cases, expr, er:errs)
            Right (cs, ex) -> caseTransformApp2 (i+1) isTop env boundVarExpr name xs ex (Prelude.concat[cases,cs]) errs
-- case with App is complex
caseTransformApp2 i isTop env boundVarExpr name ((App (Id cons) exs):xs) expr cases errs = do
    -- liftIO $ putStrLn $ "App case in caseTransformApp2: " ++ cons
    -- liftIO $ putStrLn $ "Bound var expr: " ++ ppr boundVarExpr
    -- first, run basic sanity check plus optional cases array expansion:
    let bv = if isTop then boundVarExpr else mkTupleFieldAccessExpr i boundVarExpr
    res1 <- caseTransformApp1 env bv cons exs
    case res1 of
            -- error case is terminating!!!
            Left er -> return (cases, expr, er:errs)
            -- in case all seems good and we got additional case expressions
            -- things get trickier - now we need to launch NEW
            -- caseTransformApp2 and go down a level, gather all
            -- expression changes and additional cases, and only then move on
            -- so it's a recursion inside a recursion. Will it even work???
            -- for example, we have a case:
            -- {case x of Cons (a1, Cell (b1, b2) )}
            -- we moved past a1 and encountered Cell application
            Right cs -> do
                let newBoundVarExpr = mkTupleFieldAccessExpr i boundVarExpr
                -- liftIO $ putStrLn $ "Case #: " ++ show i ++ ", Created field access: " ++ ppr newBoundVarExpr
                -- launching next level of recursion:
                (cases',expr', errs') <- caseTransformApp2 0 False env newBoundVarExpr cons exs expr [] []
                -- once we got results of the next level of recursion in ' vars, continue our current recursion:
                caseTransformApp2 (i+1) isTop env boundVarExpr name xs expr' (Prelude.concat[cases,cs,cases']) (Prelude.concat[errs',errs])
-- literal pattern inside constructor application
caseTransformApp2 i isTop env boundVarExpr name ((Lit lit):xs) expr cases errs = do
    let bv = if isTop then boundVarExpr else mkTupleFieldAccessExpr i boundVarExpr
    let checkCase = ExprLiteralCheck lit bv
    caseTransformApp2 (i+1) isTop env boundVarExpr name xs expr (cases ++ [checkCase]) errs
-- the rest is errors, so terminating
caseTransformApp2 i isTop env boundVarExpr name eee expr cases errs =
    return (cases,expr, ("Error: only constructor applications, literals, or ids are allowed inside pattern matches on the left side, and we encountered " ++ ppr eee):errs)

-- | Check sealed class exhaustiveness in pattern matches.
-- After case optimization, traverse all lambdas and warn if a PatternMatches
-- over a sealed class hierarchy is missing branches.
checkSealedExhaustiveness :: IntState ()
checkSealedExhaustiveness = do
    s <- get
    let env = currentEnvironment s
    let lambdas = topLambdas env
    mapM_ (checkLambda env) (Map.elems lambdas)
    let instLambdas = instanceLambdas env
    mapM_ (checkLambda env) (Map.elems instLambdas)
  where
    checkLambda env lam = case body lam of
        PatternMatches cases -> checkCases env lam cases
        _ -> pure ()

    checkCases env lam cases = do
        let funcName = lamName lam
        -- Extract ConsTag names from ExpandedCase patterns
        let matchedNames = Data.List.nub [nm | ExpandedCase checks _ _ <- cases
                                              , ExprConsTagCheck (ConsTag nm _) _ <- checks]
        -- Check if any matched name belongs to a sealed class hierarchy
        let sealedParents = Data.List.nub [pn | nm <- matchedNames
                                               , Just cm <- [lookupClass nm env]
                                               , Just pn <- [cmParent cm]
                                               , isSealedClass pn env]
        -- For each sealed parent, check exhaustiveness
        mapM_ (checkSealed env lam matchedNames cases) sealedParents

    checkSealed env lam matchedNames cases parentName = do
        let funcName = lamName lam
        let allChildren = Prelude.filter (/= parentName) (getAllSubclasses parentName env)
        let hasWildcard = Prelude.any isWildcard cases
        let missing = Prelude.filter (`Prelude.notElem` matchedNames) allChildren
        when (not hasWildcard && not (Prelude.null missing)) $
            logWarning (mkLogPayload (lamSrcInfo lam) ("[sealed] non-exhaustive match in " ++ funcName
                ++ ": missing " ++ Data.List.intercalate ", " missing
                ++ " from sealed class " ++ parentName ++ "\n"))

    -- A case with empty checks is a wildcard/default case
    isWildcard (ExpandedCase [] _ _) = True
    isWildcard (CaseOf [] _ _)       = True
    isWildcard _                     = False

-- ============================================================================
-- POSITIVITY CHECKING (Pass 2.1)
-- ============================================================================
-- Ensures inductive type definitions are well-founded: the type being defined
-- only appears in strictly positive positions in constructor arguments.
-- Negative occurrence (e.g., type Bad = MkBad(f: Bad -> Int)) would allow
-- non-termination and logical inconsistency.

data Polarity = Positive | Negative deriving (Eq, Show)

flipPolarity :: Polarity -> Polarity
flipPolarity Positive = Negative
flipPolarity Negative = Positive

-- | Check all sum types in the environment for strict positivity.
positivityCheckPass :: IntState ()
positivityCheckPass = do
    s <- get
    let flags = currentFlags s
    when (State.checkPositivity flags) $ do
        let env = currentEnvironment s
        mapM_ (checkTypePositivity env) (Map.toList (types env))

checkTypePositivity :: Environment -> (Name, Expr) -> IntState ()
checkTypePositivity env (typName, SumType lam) = do
    case body lam of
        Constructors cons ->
            mapM_ (checkConstructorPositivity typName) cons
        _ -> pure ()
checkTypePositivity _ _ = pure ()

checkConstructorPositivity :: Name -> Lambda -> IntState ()
checkConstructorPositivity typName conLam = do
    let conName = lamName conLam
        paramTypes = Prelude.map typ (params conLam)
    mapM_ (\paramTy ->
        case checkPositive typName Positive paramTy of
            Just path -> logWarning (mkLogPayload (lamSrcInfo conLam)
                ("[positivity] type " ++ typName ++ " occurs in negative position in constructor "
                 ++ conName ++ ": " ++ path ++ "\n"))
            Nothing -> pure ()
        ) paramTypes

-- | Check that typName occurs only in positive positions in the given expression.
-- Returns Just errorPath if a violation is found, Nothing if OK.
checkPositive :: Name -> Polarity -> Expr -> Maybe String
checkPositive typName Negative (Id n)
    | n == typName = Just (typName ++ " in negative position")
checkPositive typName Negative (App (Id n) _)
    | n == typName = Just (typName ++ " in negative position (applied)")
-- Arrow types: domain is contravariant (flipped), codomain is covariant
checkPositive typName pol (Pi _ domain codomain) =
    case checkPositive typName (flipPolarity pol) domain of
        Just path -> Just ("in function domain: " ++ path)
        Nothing -> checkPositive typName pol codomain
-- Type applications: check each argument in the current polarity
-- (conservative — a more precise check would look at variance of each param)
checkPositive typName pol (App _ args) =
    checkPositiveList typName pol args
checkPositive typName pol (NTuple fields) =
    checkPositiveList typName pol (Prelude.map snd fields)
checkPositive _ _ _ = Nothing

checkPositiveList :: Name -> Polarity -> [Expr] -> Maybe String
checkPositiveList _ _ [] = Nothing
checkPositiveList typName pol (e:es) =
    case checkPositive typName pol e of
        Just path -> Just path
        Nothing -> checkPositiveList typName pol es

-- ============================================================================
-- TERMINATION CHECKING (Pass 2.2)
-- ============================================================================
-- Checks that recursive functions make structurally decreasing calls.
-- A call is structurally decreasing if at least one argument is a strict
-- subterm of the corresponding parameter (obtained by pattern matching).

-- | Check top-level functions for termination, scoped to current module only.
-- Extracts function names from parsedModule and only checks those lambdas,
-- preventing duplicate warnings when multiple modules share the environment.
terminationCheckPass :: IntState ()
terminationCheckPass = do
    s <- get
    let flags = currentFlags s
    when (State.checkTermination flags) $ do
        let env = currentEnvironment s
        let lambdas = topLambdas env
        -- Scope to current module: only check functions defined in parsedModule
        let moduleNames = Set.fromList (extractModuleFuncNames (parsedModule s))
        let moduleLambdas = Map.filterWithKey (\k _ -> k `Set.member` moduleNames) lambdas
        mapM_ (checkFuncTermination env) (Map.toList moduleLambdas)

-- | Extract function names from parsedModule expressions.
-- Only includes top-level functions — NOT instance methods (which live in
-- instanceLambdas, not topLambdas) and NOT algebra/structure defaults.
-- Including instance method names would match the algebra's default body
-- in topLambdas (e.g., combine, !=), causing false positives.
extractModuleFuncNames :: [(Expr, SourceInfo)] -> [Name]
extractModuleFuncNames = concatMap (go . fst)
  where
    go (Function lam) = [lamName lam]
    go _ = []

checkFuncTermination :: Environment -> (Name, Lambda) -> IntState ()
checkFuncTermination env (funcName, lam) = do
    -- Skip type-level lambdas (type constructors like Maybe, List, etc.)
    -- Their return type is U _ (a universe level), not a value type.
    let isTypeLevelLam = case lamType lam of
            U _ -> True
            _   -> False
    when (not isTypeLevelLam) $ do
      -- Use the PRE-optimization body from parsedModule for subterm collection.
      -- After case optimization (Pass 2), CaseOf nodes become ExpandedCase which
      -- loses the original constructor pattern bindings. The pre-optimization body
      -- still has CaseOf with Succ(k), Z, Cons(h,t) etc. — needed for structural
      -- subterm detection.
      s <- get
      let preOptBody = lookupPreOptBody funcName (parsedModule s)
          -- Use pre-opt body for BOTH subterm collection AND call site analysis.
          -- After case optimization, variable names get replaced with field access
          -- expressions (e.g., n → tupleField(0, x)), making them unrecognizable
          -- as subterms. The pre-opt body preserves original variable names.
          bodyForCheck = maybe (body lam) id preOptBody
      -- Debug: uncomment to trace termination analysis
      -- when (funcName == "plus") $ do
      --     let paramNames2 = Set.fromList (Prelude.map name (params lam))
      --     compilerMsg Normal $ "  [DBG] plus: subterms = " ++ show (collectSubterms bodyForCheck paramNames2)
      --     compilerMsg Normal $ "  [DBG] plus: callArgs = " ++ show (Prelude.map (Prelude.map ppr) (collectRecursiveCallArgs funcName bodyForCheck))
      -- Collect all names referenced in the body
      let refs = Set.fromList (collectIdRefs bodyForCheck)
      -- Check if function is self-recursive
      when (funcName `Set.member` refs) $ do
        -- Get the function's parameter names
        let paramNames = Set.fromList (Prelude.map name (params lam))
        -- Find all pattern-destructured bindings (constructor subterms)
        let subterms = collectSubterms bodyForCheck paramNames
        -- Find all recursive call sites
        let callArgs = collectRecursiveCallArgs funcName bodyForCheck
        -- Check that at least one recursive call has a structurally smaller argument
        let allSafe = Prelude.all (hasDecreasingArg subterms) callArgs
        when (not allSafe && not (Prelude.null callArgs)) $
            logWarning (mkLogPayload (lamSrcInfo lam)
                ("[termination] function " ++ funcName
                 ++ " may not terminate: no structurally decreasing argument detected in recursive call(s)\n"))

-- | Look up a function's pre-optimization body from parsedModule.
lookupPreOptBody :: Name -> [(Expr, SourceInfo)] -> Maybe Expr
lookupPreOptBody fn [] = Nothing
lookupPreOptBody fn ((Function lam, _):rest)
    | lamName lam == fn = Just (body lam)
    | otherwise = lookupPreOptBody fn rest
lookupPreOptBody fn ((Instance _ _ _ impls _, _):rest) =
    case [body l | Function l <- impls, lamName l == fn] of
        (b:_) -> Just b
        []    -> lookupPreOptBody fn rest
lookupPreOptBody fn (_:rest) = lookupPreOptBody fn rest

-- | Collect names that are structural subterms of function parameters.
-- When we see a pattern like | Succ(n) -> ..., then n is a subterm of the
-- matched parameter.
collectSubterms :: Expr -> Set.Set Name -> Set.Set Name
collectSubterms (PatternMatches cases) paramNames =
    Set.unions (Prelude.map (`collectSubtermsCase` paramNames) cases)
collectSubterms _ _ = Set.empty

collectSubtermsCase :: Expr -> Set.Set Name -> Set.Set Name
-- CaseOf: patterns bind subterms when matching constructors
collectSubtermsCase (CaseOf pats bodyExpr _) paramNames =
    let -- Variables bound by constructor patterns whose matched value is a parameter
        newSubterms = Set.fromList [name v | v <- pats,
                                    isConstructorPattern (val v),
                                    matchesParam (val v) paramNames]
        -- Also, variables bound inside constructor applications
        innerSubterms = Set.fromList (concatMap extractInnerBindings pats)
    in Set.union newSubterms innerSubterms
-- ExpandedCase: the expanded form after pass 2
collectSubtermsCase (ExpandedCase _ bodyExpr _) paramNames =
    collectSubterms bodyExpr paramNames
collectSubtermsCase _ _ = Set.empty

-- | Check if a value is a constructor application pattern
isConstructorPattern :: Expr -> Bool
isConstructorPattern (App (Id n) _) = isUpperName n
isConstructorPattern (ConTuple _ _) = True
isConstructorPattern _ = False

-- | Check if a pattern matches (destructures) a parameter
matchesParam :: Expr -> Set.Set Name -> Bool
matchesParam _ _ = True  -- conservative: any constructor pattern creates subterms

-- | Extract variable names bound inside constructor pattern applications
extractInnerBindings :: Var -> [Name]
extractInnerBindings (Var _ _ (App (Id _) args)) =
    [n | Id n <- args, not (isUpperName n)]
extractInnerBindings (Var _ _ (ConTuple _ args)) =
    [n | Id n <- args, not (isUpperName n)]
extractInnerBindings _ = []

-- | Collect argument lists from recursive calls: f(arg1, arg2, ...)
collectRecursiveCallArgs :: Name -> Expr -> [[Expr]]
collectRecursiveCallArgs funcName = go
  where
    go (App (Id n) args) | n == funcName = [args] ++ concatMap go args
    go (App f args) = go f ++ concatMap go args
    go (NTuple fields) = concatMap (go . snd) fields
    go (ConTuple _ es) = concatMap go es
    go (BinaryOp _ e1 e2) = go e1 ++ go e2
    go (UnaryOp _ e) = go e
    go (Function lam) = go (body lam)
    go (PatternMatches cases) = concatMap go cases
    go (CaseOf _ e _) = go e
    go (ExpandedCase _ e _) = go e
    go (Typed e _) = go e
    go _ = []

-- | Check if at least one argument is a known structural subterm
hasDecreasingArg :: Set.Set Name -> [Expr] -> Bool
hasDecreasingArg subterms args =
    Prelude.any isSubtermArg args
  where
    isSubtermArg (Id n) = n `Set.member` subterms
    isSubtermArg _ = False

isUpperName :: Name -> Bool
isUpperName [] = False
isUpperName (c:_) = c >= 'A' && c <= 'Z'

-- ============================================================================
-- PATTERN MATCH COVERAGE CHECKING (Pass 2.3)
-- ============================================================================
-- Checks that pattern matches over algebraic data types are exhaustive:
-- all constructors of the matched type are covered, or a wildcard/default
-- case is present.

-- | Check all functions for pattern match coverage.
coverageCheckPass :: IntState ()
coverageCheckPass = do
    s <- get
    let flags = currentFlags s
    when (State.checkCoverage flags) $ do
        let env = currentEnvironment s
        let lambdas = topLambdas env
        mapM_ (checkLambdaCoverage env) (Map.elems lambdas)
        let instLambdas = instanceLambdas env
        mapM_ (checkLambdaCoverage env) (Map.elems instLambdas)

checkLambdaCoverage :: Environment -> Lambda -> IntState ()
checkLambdaCoverage env lam = case body lam of
    PatternMatches cases -> checkCasesCoverage env lam cases
    _ -> pure ()

checkCasesCoverage :: Environment -> Lambda -> [Expr] -> IntState ()
checkCasesCoverage env lam cases = do
    -- Check if there's a wildcard/default case
    let hasWildcard = Prelude.any isCoverageWildcard cases
    if hasWildcard
        then pure ()  -- wildcard covers everything
        else do
            -- Extract matched constructor names from all cases
            let matchedCons = Data.List.nub [nm | ExpandedCase checks _ _ <- cases
                                                , ExprConsTagCheck (ConsTag nm _) _ <- checks]
            -- For each matched constructor, find its parent type
            let parentTypes = Data.List.nub [pn | nm <- matchedCons
                                                , Just pn <- [lookupTypeOfConstructor nm env]]
            -- For each parent type, check exhaustiveness
            mapM_ (checkTypeCoverage env lam matchedCons) parentTypes

checkTypeCoverage :: Environment -> Lambda -> [Name] -> Name -> IntState ()
checkTypeCoverage env lam matchedCons typName = do
    let funcName = lamName lam
    -- Get all constructors of this type
    case lookupType typName env of
        Just (SumType typLam) -> case body typLam of
            Constructors cons -> do
                let allConsNames = Prelude.map lamName cons
                    missing = Prelude.filter (`Prelude.notElem` matchedCons) allConsNames
                when (not (Prelude.null missing)) $
                    logWarning (mkLogPayload (lamSrcInfo lam)
                        ("[coverage] non-exhaustive pattern match in " ++ funcName
                         ++ ": missing " ++ Data.List.intercalate ", " missing
                         ++ " from type " ++ typName ++ "\n"))
            _ -> pure ()
        _ -> pure ()

-- A case with empty checks is a wildcard/default case
isCoverageWildcard :: Expr -> Bool
isCoverageWildcard (ExpandedCase [] _ _) = True
isCoverageWildcard (CaseOf [] _ _)       = True
isCoverageWildcard _                     = False
