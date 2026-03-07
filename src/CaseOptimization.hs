{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

-- | Pass 2: Case Optimization
-- Expands PatternMatches into ExpandedCase with flattened constructor tag
-- and literal checks. Handles nested constructor patterns via double-recursive
-- descent (caseTransformApp1/App2).
module CaseOptimization
    ( caseOptimizationPass
    , checkSealedExhaustiveness
    ) where

import State
import Surface
import Logs

import Control.Monad (when)
import Control.Monad.Trans.State.Strict
import Util.PrettyPrinting as TC

import Data.HashMap.Strict as Map
import qualified Data.List

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
    where f k lam@(Lambda nm args (PatternMatches exs) tp _) = do
                exs' <- mapM (expandCase lam) exs
                return lam {body = PatternMatches exs'}
          f k e = return e

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
        PatternMatches cases -> checkCases env (lamName lam) cases
        _ -> pure ()

    checkCases env funcName cases = do
        -- Extract ConsTag names from ExpandedCase patterns
        let matchedNames = Data.List.nub [nm | ExpandedCase checks _ _ <- cases
                                              , ExprConsTagCheck (ConsTag nm _) _ <- checks]
        -- Check if any matched name belongs to a sealed class hierarchy
        let sealedParents = Data.List.nub [pn | nm <- matchedNames
                                               , Just cm <- [lookupClass nm env]
                                               , Just pn <- [cmParent cm]
                                               , isSealedClass pn env]
        -- For each sealed parent, check exhaustiveness
        mapM_ (checkSealed env funcName matchedNames cases) sealedParents

    checkSealed env funcName matchedNames cases parentName = do
        let allChildren = Prelude.filter (/= parentName) (getAllSubclasses parentName env)
        let hasWildcard = Prelude.any isWildcard cases
        let missing = Prelude.filter (`Prelude.notElem` matchedNames) allChildren
        when (not hasWildcard && not (Prelude.null missing)) $
            logWarning (LogPayload 0 0 "" ("[sealed] non-exhaustive match in " ++ funcName
                ++ ": missing " ++ Data.List.intercalate ", " missing
                ++ " from sealed class " ++ parentName ++ "\n"))

    -- A case with empty checks is a wildcard/default case
    isWildcard (ExpandedCase [] _ _) = True
    isWildcard (CaseOf [] _ _)       = True
    isWildcard _                     = False
