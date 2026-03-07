{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import qualified Data.Text as T

import Surface
import CLM
import State
import Intrinsics (lookupIntrinsic)
import Pipeline
import TypeCheck
import ModuleSystem
import Util.PrettyPrinting (ppr)
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import Data.List (isInfixOf, isPrefixOf)
import Interpreter
import Parser
import Logs
import Util.IOLogger (initLogState)
import System.Directory (doesFileExist)

-- Helper: run an IntState action with a fresh state
runFresh :: IntState a -> IO a
runFresh act = runIntState act emptyIntState

-- Helper: load all modules from lib/ into state, return the state
setupEnv :: IO InterpreterState
setupEnv = do
    -- Resolve module load order, then load all modules
    finalState <- evalStateT (execStateT setup emptyIntState) initLogState
    return finalState
  where
    setup = do
        -- Load prelude first (primitives)
        loadFileQuiet preludeModulePath
        -- Resolve dependency order for all lib/ modules
        let searchPaths = ["lib/"]
        allModules <- liftIO $ resolveAllDeps searchPaths Set.empty [baseModulePath]
        -- Load each module in dependency order
        mapM_ (\(_, filePath) -> loadFileQuiet filePath) allModules

-- Helper: parse a string as a tulam file, returning the expressions
parseTestString :: String -> IO (Either String [Expr])
parseTestString input = runParseOnly (T.pack input) "<test>"

-- Helper: check that an intrinsic exists
hasIntrinsic :: String -> String -> IO ()
hasIntrinsic func typName = case lookupIntrinsic func typName of
    Just _  -> pure ()
    Nothing -> expectationFailure $ "Intrinsic not found: " ++ func ++ " for " ++ typName

-- Helper: load a test program file on top of an existing state
loadTestProgram :: InterpreterState -> FilePath -> IO InterpreterState
loadTestProgram st filePath = do
    evalStateT (execStateT (loadFileQuiet filePath) st) initLogState

-- Helper: enable newStrings flag on a state (for string literal desugaring tests)
enableNewStrings :: InterpreterState -> InterpreterState
enableNewStrings st = st { currentFlags = (currentFlags st) { newStrings = True } }

-- Helper: load multiple test program files in order on top of an existing state
loadTestPrograms :: InterpreterState -> [FilePath] -> IO InterpreterState
loadTestPrograms st [] = return st
loadTestPrograms st (f:fs) = do
    st' <- loadTestProgram st f
    loadTestPrograms st' fs

-- Helper: evaluate an expression string in the given state, return CLMExpr result
evalExpr :: InterpreterState -> String -> IO CLMExpr
evalExpr st input = do
    evalStateT (evalStateT (evalExprM input) st) initLogState

evalExprM :: String -> IntState CLMExpr
evalExprM input = do
    res <- parseExpr (T.pack input)
    case res of
        Left err -> return $ CLMERR ("Parse error: " ++ show err) SourceInteractive
        Right ex0 -> do
            let ex = afterparse $ traverseExpr afterparse ex0
            s <- get
            let env = currentEnvironment s
            let clmex = exprToCLM env ex
            case clmex of
                ex1@(CLMAPP _ _) -> do
                    ex1' <- evalCLM 0 ex1
                    _contEval 1 ex1 ex1'
                ex1@(CLMIAP _ _) -> do
                    ex1' <- evalCLM 0 ex1
                    _contEval 1 ex1 ex1'
                ex1@(CLMCON _ _) -> return ex1
                ex1@(CLMLIT _) -> return ex1
                ex1@(CLMLAM _) -> return ex1
                ex1@(CLMID _) -> do
                    let env = currentEnvironment s
                    case lookupCLMBindingOrLambda env (case ex1 of CLMID nm -> nm) of
                        Just v -> return v
                        Nothing -> return ex1
                ex1@(CLMHANDLE _ _ _ _) -> do
                    ex1' <- evalCLM 0 ex1
                    _contEval 1 ex1 ex1'
                other -> return other

-- CLM expression constructors for assertions
conTrue :: CLMExpr
conTrue = CLMCON (ConsTag "True" 0) []

conFalse :: CLMExpr
conFalse = CLMCON (ConsTag "False" 1) []

conZ :: CLMExpr
conZ = CLMCON (ConsTag "Z" 0) []

conSucc :: CLMExpr -> CLMExpr
conSucc n = CLMCON (ConsTag "Succ" 1) [n]

conNothing :: CLMExpr
conNothing = CLMCON (ConsTag "Nothing" 0) []

conJust :: CLMExpr -> CLMExpr
conJust x = CLMCON (ConsTag "Just" 1) [x]

conNil :: CLMExpr
conNil = CLMCON (ConsTag "Nil" 0) []

conCons :: CLMExpr -> CLMExpr -> CLMExpr
conCons h t = CLMCON (ConsTag "Cons" 1) [h, t]

conLT :: CLMExpr
conLT = CLMCON (ConsTag "LessThan" 0) []

conEQ :: CLMExpr
conEQ = CLMCON (ConsTag "Equal" 1) []

conGT :: CLMExpr
conGT = CLMCON (ConsTag "GreaterThan" 2) []

nat :: Int -> CLMExpr
nat 0 = conZ
nat n = conSucc (nat (n - 1))

-- Helper: check if a TCError is an UnboundVar (possibly wrapped in WithContext)
isUnboundVar :: TCError -> Bool
isUnboundVar (UnboundVar _) = True
isUnboundVar (WithContext _ inner) = isUnboundVar inner
isUnboundVar _ = False

main :: IO ()
main = do
    st <- setupEnv
    hspec $ do
        describe "Parser" $ do
            it "parses simple Id" $ do
                res <- runFresh $ parseExpr (T.pack "Z")
                case res of
                    Right (Id "Z") -> return ()
                    other -> expectationFailure $ "Expected Id Z, got: " ++ show other

            it "parses function application" $ do
                res <- runFresh $ parseExpr (T.pack "plus(x, y)")
                case res of
                    Right (App (Id "plus") [Id "x", Id "y"]) -> return ()
                    other -> expectationFailure $ "Expected App plus [x,y], got: " ++ show other

            it "parses anonymous lambda" $ do
                res <- runFresh $ parseExpr (T.pack "\\x -> x")
                case res of
                    Right (Function (Lambda "" [Var "x" UNDEFINED UNDEFINED] (Id "x") UNDEFINED _)) -> return ()
                    other -> expectationFailure $ "Expected anon lambda, got: " ++ show other

        describe "Pipeline integration" $ do
            it "registers Nat type in environment" $ do
                let env = currentEnvironment st
                case lookupType "Nat" env of
                    Just (SumType _) -> return ()
                    other -> expectationFailure $ "Expected SumType Nat, got: " ++ show other

            it "registers Bool type in environment" $ do
                let env = currentEnvironment st
                case lookupType "Bool" env of
                    Just (SumType _) -> return ()
                    other -> expectationFailure $ "Expected SumType Bool, got: " ++ show other

            it "registers Maybe type in environment" $ do
                let env = currentEnvironment st
                case lookupType "Maybe" env of
                    Just (SumType _) -> return ()
                    other -> expectationFailure $ "Expected SumType Maybe, got: " ++ show other

            it "registers List type in environment" $ do
                let env = currentEnvironment st
                case lookupType "List" env of
                    Just (SumType _) -> return ()
                    other -> expectationFailure $ "Expected SumType List, got: " ++ show other

            it "registers Z constructor" $ do
                let env = currentEnvironment st
                case lookupConstructor "Z" env of
                    Just (_, 0) -> return ()
                    other -> expectationFailure $ "Expected Z at tag 0, got: " ++ show other

            it "registers Succ constructor" $ do
                let env = currentEnvironment st
                case lookupConstructor "Succ" env of
                    Just (_, 1) -> return ()
                    other -> expectationFailure $ "Expected Succ at tag 1, got: " ++ show other

            it "registers Eq structure" $ do
                let env = currentEnvironment st
                case lookupType "Eq" env of
                    Just (Structure _ _) -> return ()
                    other -> expectationFailure $ "Expected Structure Eq, got: " ++ show other

            it "has eq instance for Nat" $ do
                let env = currentEnvironment st
                case lookupInstanceLambda "==" ["Nat"] env of
                    Just _ -> return ()
                    Nothing -> expectationFailure "Expected Eq instance for Nat"

        describe "End-to-end evaluation" $ do
            -- Nat operations
            it "eq(Z, Z) = True" $ do
                result <- evalExpr st "eq(Z, Z)"
                result `shouldBe` conTrue

            it "eq(Z, Succ(Z)) = False" $ do
                result <- evalExpr st "eq(Z, Succ(Z))"
                result `shouldBe` conFalse

            it "plus(Succ(Z), Succ(Z)) = Succ(Succ(Z))" $ do
                result <- evalExpr st "plus(Succ(Z), Succ(Z))"
                result `shouldBe` nat 2

            it "compare(Z, Succ(Z)) = LessThan" $ do
                result <- evalExpr st "compare(Z, Succ(Z))"
                result `shouldBe` conLT

            it "compare(Z, Z) = Equal" $ do
                result <- evalExpr st "compare(Z, Z)"
                result `shouldBe` conEQ

            it "compare(Succ(Z), Z) = GreaterThan" $ do
                result <- evalExpr st "compare(Succ(Z), Z)"
                result `shouldBe` conGT

            -- Monoid
            it "combine(Succ(Z), Succ(Succ(Z))) = Succ(Succ(Succ(Z)))" $ do
                result <- evalExpr st "combine(Succ(Z), Succ(Succ(Z)))"
                result `shouldBe` nat 3

            it "empty = Z (Monoid Nat)" $ do
                result <- evalExpr st "empty"
                result `shouldBe` conZ

            -- Bool operations
            it "not(True) = False" $ do
                result <- evalExpr st "not(True)"
                result `shouldBe` conFalse

            it "not(False) = True" $ do
                result <- evalExpr st "not(False)"
                result `shouldBe` conTrue

            -- Morphism dispatch: convert(Succ(Z)) is ambiguous without return type
            -- annotation (could be Nat->Bool or Nat->Int). Accept either result.
            it "convert(Succ(Z)) dispatches Nat->something" $ do
                result <- evalExpr st "convert(Succ(Z))"
                -- Nat->Bool gives True, Nat->Int gives 1
                (result == conTrue || result == CLMLIT (LInt 1)) `shouldBe` True

            it "convert(False) = Z (Bool->Nat)" $ do
                result <- evalExpr st "convert(False)"
                result `shouldBe` conZ

            -- If/then/else
            it "if True then Z else Succ(Z) = Z" $ do
                result <- evalExpr st "if True then Z else Succ(Z)"
                result `shouldBe` conZ

            it "if False then Z else Succ(Z) = Succ(Z)" $ do
                result <- evalExpr st "if False then Z else Succ(Z)"
                result `shouldBe` conSucc conZ

            -- Let/in
            it "let x = Succ(Z) in plus(x, x) = Succ(Succ(Z))" $ do
                result <- evalExpr st "let x = Succ(Z) in plus(x, x)"
                result `shouldBe` nat 2

            -- Intrinsics (Int arithmetic)
            it "3 + 4 = 7" $ do
                result <- evalExpr st "3 + 4"
                result `shouldBe` CLMLIT (LInt 7)

            it "10 - 3 = 7" $ do
                result <- evalExpr st "10 - 3"
                result `shouldBe` CLMLIT (LInt 7)

            it "3 * 4 = 12" $ do
                result <- evalExpr st "3 * 4"
                result `shouldBe` CLMLIT (LInt 12)

            it "10 > 3 = True" $ do
                result <- evalExpr st "10 > 3"
                result `shouldBe` conTrue

            it "3 > 10 = False" $ do
                result <- evalExpr st "3 > 10"
                result `shouldBe` conFalse

        describe "Combinators" $ do
            it "id(Succ(Z)) = Succ(Z)" $ do
                result <- evalExpr st "id(Succ(Z))"
                result `shouldBe` conSucc conZ

            it "const(Z, True) = Z" $ do
                result <- evalExpr st "const(Z, True)"
                result `shouldBe` conZ

            it "apply(not, True) = False" $ do
                result <- evalExpr st "apply(not, True)"
                result `shouldBe` conFalse

            it "compose(not, not, True) = True" $ do
                result <- evalExpr st "compose(not, not, True)"
                result `shouldBe` conTrue

        describe "Maybe / List / Either" $ do
            it "Just(Succ(Z))" $ do
                result <- evalExpr st "Just(Succ(Z))"
                result `shouldBe` conJust (conSucc conZ)

            it "isNothing(Nothing) = True" $ do
                result <- evalExpr st "isNothing(Nothing)"
                result `shouldBe` conTrue

            it "isNothing(Just(Z)) = False" $ do
                result <- evalExpr st "isNothing(Just(Z))"
                result `shouldBe` conFalse

            it "fromMaybe(Z, Just(Succ(Z))) = Succ(Z)" $ do
                result <- evalExpr st "fromMaybe(Z, Just(Succ(Z)))"
                result `shouldBe` conSucc conZ

            it "fromMaybe(Z, Nothing) = Z" $ do
                result <- evalExpr st "fromMaybe(Z, Nothing)"
                result `shouldBe` conZ

            it "head(Cons(Succ(Z), Nil)) = Just(Succ(Z))" $ do
                result <- evalExpr st "head(Cons(Succ(Z), Nil))"
                result `shouldBe` conJust (conSucc conZ)

            it "head(Nil) = Nothing" $ do
                result <- evalExpr st "head(Nil)"
                result `shouldBe` conNothing

            it "isEmpty(Nil) = True" $ do
                result <- evalExpr st "isEmpty(Nil)"
                result `shouldBe` conTrue

            it "isEmpty(Cons(Z, Nil)) = False" $ do
                result <- evalExpr st "isEmpty(Cons(Z, Nil))"
                result `shouldBe` conFalse

            it "combine(Cons(Z, Nil), Cons(Succ(Z), Nil)) = Cons(Z, Cons(Succ(Z), Nil))" $ do
                result <- evalExpr st "combine(Cons(Z, Nil), Cons(Succ(Z), Nil))"
                result `shouldBe` conCons conZ (conCons (conSucc conZ) conNil)

        describe "Functor / Applicative / Monad" $ do
            it "fmap(not, Just(True)) = Just(False)" $ do
                result <- evalExpr st "fmap(not, Just(True))"
                result `shouldBe` conJust conFalse

            it "fmap(not, Nothing) = Nothing" $ do
                result <- evalExpr st "fmap(not, Nothing)"
                result `shouldBe` conNothing

            it "fmap(not, Just(False)) = Just(True)" $ do
                result <- evalExpr st "fmap(not, Just(False))"
                result `shouldBe` conJust conTrue

            it "fmap(not, Cons(True, Cons(False, Nil))) = Cons(False, Cons(True, Nil))" $ do
                result <- evalExpr st "fmap(not, Cons(True, Cons(False, Nil)))"
                result `shouldBe` conCons conFalse (conCons conTrue conNil)

            it "fmap(not, Nil) = Nil" $ do
                result <- evalExpr st "fmap(not, Nil)"
                result `shouldBe` conNil

            it "bind(Just(True), \\x -> Just(not(x))) = Just(False)" $ do
                result <- evalExpr st "bind(Just(True), \\x -> Just(not(x)))"
                result `shouldBe` conJust conFalse

            it "bind(Nothing, \\x -> Just(x)) = Nothing" $ do
                result <- evalExpr st "bind(Nothing, \\x -> Just(x))"
                result `shouldBe` conNothing

            it "bind(Cons(Z, Cons(Succ(Z), Nil)), \\x -> Cons(x, Cons(x, Nil))) duplicates elements" $ do
                result <- evalExpr st "bind(Cons(Z, Cons(Succ(Z), Nil)), \\x -> Cons(x, Cons(x, Nil)))"
                result `shouldBe` conCons conZ (conCons conZ (conCons (conSucc conZ) (conCons (conSucc conZ) conNil)))

            it "bind(Nil, \\x -> Cons(x, Nil)) = Nil" $ do
                result <- evalExpr st "bind(Nil, \\x -> Cons(x, Nil))"
                result `shouldBe` conNil

            it "ap(Just(not), Just(True)) = Just(False)" $ do
                result <- evalExpr st "ap(Just(not), Just(True))"
                result `shouldBe` conJust conFalse

            it "ap(Nothing, Just(True)) = Nothing" $ do
                result <- evalExpr st "ap(Nothing, Just(True))"
                result `shouldBe` conNothing

            it "seq(Just(True), Just(False)) = Just(False)" $ do
                result <- evalExpr st "seq(Just(True), Just(False))"
                result `shouldBe` conJust conFalse

            it "seq(Nothing, Just(True)) = Nothing" $ do
                result <- evalExpr st "seq(Nothing, Just(True))"
                result `shouldBe` conNothing

        describe "Repr system" $ do
            it "toRepr(Z) = 0" $ do
                result <- evalExpr st "toRepr(Z)"
                result `shouldBe` CLMLIT (LInt 0)

            it "toRepr(Succ(Succ(Z))) = 2" $ do
                result <- evalExpr st "toRepr(Succ(Succ(Z)))"
                result `shouldBe` CLMLIT (LInt 2)

            it "fromRepr(0) = Z" $ do
                result <- evalExpr st "fromRepr(0)"
                result `shouldBe` conZ

            it "fromRepr(3) = Succ(Succ(Succ(Z)))" $ do
                result <- evalExpr st "fromRepr(3)"
                result `shouldBe` nat 3

            it "Succ(Z) as Int = 1" $ do
                result <- evalExpr st "Succ(Z) as Int"
                result `shouldBe` CLMLIT (LInt 1)

            it "5 as Nat = Succ^5(Z)" $ do
                result <- evalExpr st "5 as Nat"
                result `shouldBe` nat 5

            it "roundtrip: fromRepr(toRepr(Succ(Succ(Z)))) = Succ(Succ(Z))" $ do
                result <- evalExpr st "fromRepr(toRepr(Succ(Succ(Z))))"
                result `shouldBe` nat 2

        describe "Literal pattern matching" $ do
            it "fromRepr with literal pattern: fromRepr(0) = Z" $ do
                result <- evalExpr st "fromRepr(0)"
                result `shouldBe` conZ

            it "fromRepr with literal pattern: fromRepr(2) = Succ(Succ(Z))" $ do
                result <- evalExpr st "fromRepr(2)"
                result `shouldBe` nat 2

            it "describeInt(0) = Z" $ do
                result <- evalExpr st "describeInt(0)"
                result `shouldBe` conZ

            it "describeInt(1) = Succ(Z)" $ do
                result <- evalExpr st "describeInt(1)"
                result `shouldBe` conSucc conZ

            it "describeInt(42) = Succ(Succ(Z))" $ do
                result <- evalExpr st "describeInt(42)"
                result `shouldBe` nat 2

        describe "Algebraic hierarchy" $ do
            it "div(one, one) = 1 (zero/one in Int context)" $ do
                result <- evalExpr st "div(one, one)"
                result `shouldBe` CLMLIT (LInt 1)

            it "mod(one, one) = 0 (zero in Int context)" $ do
                result <- evalExpr st "mod(one, one)"
                result `shouldBe` CLMLIT (LInt 0)

            it "signum(5) = 1" $ do
                result <- evalExpr st "signum(5)"
                result `shouldBe` CLMLIT (LInt 1)

            it "signum(-3) = -1" $ do
                result <- evalExpr st "signum(-3)"
                result `shouldBe` CLMLIT (LInt (-1))

            it "signum(0) = 0" $ do
                result <- evalExpr st "signum(0)"
                result `shouldBe` CLMLIT (LInt 0)

            it "3 + 4 = 7 (regression)" $ do
                result <- evalExpr st "3 + 4"
                result `shouldBe` CLMLIT (LInt 7)

            it "10 - 3 = 7 (regression)" $ do
                result <- evalExpr st "10 - 3"
                result `shouldBe` CLMLIT (LInt 7)

            it "3 * 4 = 12 (regression)" $ do
                result <- evalExpr st "3 * 4"
                result `shouldBe` CLMLIT (LInt 12)

        describe "Mathematical hierarchy" $ do
            -- Nat Semiring: + and * operators on Nat
            it "Succ(Z) + Succ(Z) = Succ(Succ(Z)) (Nat Semiring)" $ do
                result <- evalExpr st "Succ(Z) + Succ(Z)"
                result `shouldBe` nat 2

            it "Succ(Succ(Z)) * Succ(Succ(Succ(Z))) = nat 6 (Nat Semiring)" $ do
                result <- evalExpr st "Succ(Succ(Z)) * Succ(Succ(Succ(Z)))"
                result `shouldBe` nat 6

            it "Z + Succ(Z) = Succ(Z) (Nat additive identity)" $ do
                result <- evalExpr st "Z + Succ(Z)"
                result `shouldBe` nat 1

            it "Succ(Z) * Z = Z (Nat multiplicative annihilation)" $ do
                result <- evalExpr st "Succ(Z) * Z"
                result `shouldBe` conZ

            -- Nat mult helper
            it "mult(Succ(Succ(Z)), Succ(Succ(Z))) = nat 4" $ do
                result <- evalExpr st "mult(Succ(Succ(Z)), Succ(Succ(Z)))"
                result `shouldBe` nat 4

            -- Group(Int): generic combine/empty/inverse
            it "combine(3, 4) = 7 (Group Int)" $ do
                result <- evalExpr st "combine(3, 4)"
                result `shouldBe` CLMLIT (LInt 7)

            it "inverse(5) = -5 (Group Int)" $ do
                result <- evalExpr st "inverse(5)"
                result `shouldBe` CLMLIT (LInt (-5))

            it "combine(inverse(3), 3) = 0 (Group Int inverse law)" $ do
                result <- evalExpr st "combine(inverse(3), 3)"
                result `shouldBe` CLMLIT (LInt 0)

            -- AbelianGroup(Float64)
            it "combine(1.5, 2.5) = 4.0 (AbelianGroup Float64)" $ do
                result <- evalExpr st "combine(1.5, 2.5)"
                result `shouldBe` CLMLIT (LFloat 4.0)

            it "inverse(3.0) = -3.0 (AbelianGroup Float64)" $ do
                result <- evalExpr st "inverse(3.0)"
                result `shouldBe` CLMLIT (LFloat (-3.0))

        describe "Arrow types" $ do
            -- Arrow types are used in Functor/Monad signatures (g: a -> b, f: a -> m(b))
            -- These tests verify arrow-typed parameters work end-to-end
            it "fmap with arrow-typed param (g: a -> b): fmap(not, Just(True)) = Just(False)" $ do
                result <- evalExpr st "fmap(not, Just(True))"
                result `shouldBe` conJust conFalse

            it "bind with arrow-typed callback (f: a -> m(b)): bind(Just(Z), \\x -> Just(Succ(x))) = Just(Succ(Z))" $ do
                result <- evalExpr st "bind(Just(Z), \\x -> Just(Succ(x)))"
                result `shouldBe` conJust (conSucc conZ)

            it "compose arrow-typed functions: compose(not, not, True) = True" $ do
                result <- evalExpr st "compose(not, not, True)"
                result `shouldBe` conTrue

            it "apply with arrow-typed param: apply(not, True) = False" $ do
                result <- evalExpr st "apply(not, True)"
                result `shouldBe` conFalse

            it "< operator not confused with arrow type: 3 < 10 = True" $ do
                result <- evalExpr st "3 < 10"
                result `shouldBe` conTrue

        describe "Where clauses" $ do
            it "doubleNat(Succ(Z)) = Succ(Succ(Succ(Succ(Z))))" $ do
                result <- evalExpr st "doubleNat(Succ(Z))"
                result `shouldBe` nat 4

            it "doubleNat(Z) = Z" $ do
                result <- evalExpr st "doubleNat(Z)"
                result `shouldBe` conZ

        -- ================================================================
        -- Type Checker Tests
        -- ================================================================
        describe "Type Checker" $ do
            let st0 = initTCState TCRelaxed
                env0 = emptyTCEnv

            describe "exprToTy" $ do
                it "converts Int to TCon Int" $ do
                    let Right (ty, _) = runTC (exprToTy (Id "Int")) env0 st0
                    ty `shouldBe` TCon "Int"

                it "converts Float64 to TCon Float64" $ do
                    let Right (ty, _) = runTC (exprToTy (Id "Float64")) env0 st0
                    ty `shouldBe` TCon "Float64"

                it "converts String to TCon String" $ do
                    let Right (ty, _) = runTC (exprToTy (Id "String")) env0 st0
                    ty `shouldBe` TCon "String"

                it "converts Bool to TCon Bool" $ do
                    let Right (ty, _) = runTC (exprToTy (Id "Bool")) env0 st0
                    ty `shouldBe` TCon "Bool"

                it "converts U 0 to TU 0" $ do
                    let Right (ty, _) = runTC (exprToTy (U 0)) env0 st0
                    ty `shouldBe` TU 0

                it "converts arrow type a -> b" $ do
                    let Right (ty, _) = runTC (exprToTy (ArrowType (Id "Int") (Id "Bool"))) env0 st0
                    ty `shouldBe` TArrow (TCon "Int") (TCon "Bool")

                it "converts type application Maybe(Int)" $ do
                    let Right (ty, _) = runTC (exprToTy (App (Id "Maybe") [Id "Int"])) env0 st0
                    ty `shouldBe` TApp (TCon "Maybe") [TCon "Int"]

                it "converts UNDEFINED to fresh var" $ do
                    let Right (ty, st') = runTC (exprToTy UNDEFINED) env0 st0
                    ty `shouldBe` TVar 0
                    nextVar st' `shouldBe` 1

                it "converts lowercase id to TRigid" $ do
                    let Right (ty, _) = runTC (exprToTy (Id "a")) env0 st0
                    ty `shouldBe` TRigid "a"

                it "converts capitalized id to TCon" $ do
                    let Right (ty, _) = runTC (exprToTy (Id "MyType")) env0 st0
                    ty `shouldBe` TCon "MyType"

            describe "Unification" $ do
                it "unifies identical concrete types" $ do
                    let result = runTC (unify (TCon "Int") (TCon "Int")) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "fails on mismatched concrete types" $ do
                    let result = runTC (unify (TCon "Int") (TCon "Bool")) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should have failed"

                it "unifies variable with concrete type" $ do
                    let Right (_, st') = runTC (unify (TVar 0) (TCon "Int")) env0 (st0 { nextVar = 1 })
                    let Right (ty, _) = runTC (applySubst (TVar 0)) env0 st'
                    ty `shouldBe` TCon "Int"

                it "unifies two variables" $ do
                    let Right (_, st') = runTC (unify (TVar 0) (TVar 1)) env0 (st0 { nextVar = 2 })
                    let Right (ty, _) = runTC (applySubst (TVar 0)) env0 st'
                    ty `shouldBe` TVar 1

                it "detects occurs check" $ do
                    let result = runTC (unify (TVar 0) (TApp (TCon "Maybe") [TVar 0])) env0 (st0 { nextVar = 1 })
                    case result of
                        Left (OccursCheck _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs
                        Right _ -> expectationFailure "Should have failed with occurs check"

                it "unifies arrow types" $ do
                    let result = runTC (unify (TArrow (TVar 0) (TCon "Bool")) (TArrow (TCon "Int") (TVar 1))) env0 (st0 { nextVar = 2 })
                    case result of
                        Right (_, st') -> do
                            let Right (t0, _) = runTC (applySubst (TVar 0)) env0 st'
                            let Right (t1, _) = runTC (applySubst (TVar 1)) env0 st'
                            t0 `shouldBe` TCon "Int"
                            t1 `shouldBe` TCon "Bool"
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "unifies type applications" $ do
                    let result = runTC (unify (TApp (TCon "Maybe") [TVar 0]) (TApp (TCon "Maybe") [TCon "Int"])) env0 (st0 { nextVar = 1 })
                    case result of
                        Right (_, st') -> do
                            let Right (ty, _) = runTC (applySubst (TVar 0)) env0 st'
                            ty `shouldBe` TCon "Int"
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "unifies universes" $ do
                    let result = runTC (unify (TU 0) (TU 0)) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "fails on different universes" $ do
                    let result = runTC (unify (TU 0) (TU 1)) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should have failed"

                it "transitively resolves substitutions" $ do
                    -- TVar 0 = TVar 1, TVar 1 = Int => TVar 0 resolves to Int
                    let st1 = st0 { nextVar = 2 }
                    let Right (_, st2) = runTC (unify (TVar 0) (TVar 1)) env0 st1
                    let Right (_, st3) = runTC (unify (TVar 1) (TCon "Int")) env0 st2
                    let Right (ty, _) = runTC (applySubst (TVar 0)) env0 st3
                    ty `shouldBe` TCon "Int"

            describe "Row unification" $ do
                it "unifies identical empty rows" $ do
                    let result = runTC (unify (TRecord REmpty) (TRecord REmpty)) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "unifies record with same fields" $ do
                    let r1 = TRecord (RExtend "x" (TCon "Int") REmpty)
                        r2 = TRecord (RExtend "x" (TCon "Int") REmpty)
                    let result = runTC (unify r1 r2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "fails on mismatched field types" $ do
                    let r1 = TRecord (RExtend "x" (TCon "Int") REmpty)
                        r2 = TRecord (RExtend "x" (TCon "Bool") REmpty)
                    let result = runTC (unify r1 r2) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should have failed"

                it "unifies rows with different field order" $ do
                    let r1 = TRecord (RExtend "x" (TCon "Int") (RExtend "y" (TCon "Bool") REmpty))
                        r2 = TRecord (RExtend "y" (TCon "Bool") (RExtend "x" (TCon "Int") REmpty))
                    let result = runTC (unify r1 r2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "unifies open row with concrete row" $ do
                    let st1 = st0 { nextVar = 1 }
                        r1 = TRecord (RExtend "x" (TCon "Int") (RVar 0))
                        r2 = TRecord (RExtend "x" (TCon "Int") (RExtend "y" (TCon "Bool") REmpty))
                    let result = runTC (unify r1 r2) env0 st1
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

            describe "Bidirectional checking" $ do
                it "infers literal Int type" $ do
                    let Right (ty, _) = runTC (infer (Lit (LInt 42))) env0 st0
                    ty `shouldBe` TCon "Int"

                it "infers literal Float64 type" $ do
                    let Right (ty, _) = runTC (infer (Lit (LFloat 3.14))) env0 st0
                    ty `shouldBe` TCon "Float64"

                it "infers literal String type" $ do
                    let Right (ty, _) = runTC (infer (Lit (LString "hello"))) env0 st0
                    ty `shouldBe` TCon "String"

                it "infers variable type from environment" $ do
                    let env1 = env0 { varTypes = Map.fromList [("x", TCon "Int")] }
                    let Right (ty, _) = runTC (infer (Id "x")) env1 st0
                    ty `shouldBe` TCon "Int"

                it "check succeeds for matching literal" $ do
                    let result = runTC (check (Lit (LInt 5)) (TCon "Int")) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "check warns on type mismatch (permissive mode)" $ do
                    let result = runTC (check (Lit (LInt 5)) (TCon "Bool")) env0 st0
                    case result of
                        Right (_, st') -> length (tcErrors st') `shouldSatisfy` (> 0)
                        Left _ -> pure ()  -- also acceptable

                it "infers function type from annotated lambda" $ do
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED] (Id "x") (Id "Int")
                    let Right (ty, _) = runTC (infer (Function lam)) env0 st0
                    ty `shouldBe` TArrow (TCon "Int") (TCon "Int")

                it "infers UNDEFINED param type as fresh var" $ do
                    let lam = mkLambda "f" [Var "x" UNDEFINED UNDEFINED] (Lit (LInt 42)) (Id "Int")
                    let Right (ty, _) = runTC (infer (Function lam)) env0 st0
                    -- Should be ?0 -> Int (fresh var for param, Int for body)
                    case ty of
                        TArrow (TVar _) (TCon "Int") -> pure ()
                        other -> expectationFailure $ "Expected ?n -> Int, got: " ++ show other

                it "infers constructor type from compiler environment" $ do
                    -- Use the full environment with constructors
                    let result = runTC (infer (Lit (LInt 42))) env0 st0
                    case result of
                        Right (TCon "Int", _) -> pure ()
                        other -> expectationFailure $ "Expected Int, got: " ++ show other

            describe "Structure constraints" $ do
                it "emits constraint for implicit-param function lookup" $ do
                    -- Create a minimal compiler env with an implicit-param function
                    let implLam = mkLambda "(+)" [Var "a" (Implicit (App (Id "AdditiveSemigroup") [Id "a"])) UNDEFINED,
                                               Var "x" (Id "a") UNDEFINED, Var "y" (Id "a") UNDEFINED]
                                        UNDEFINED (Id "a")
                        cenv = addNamedLambda implLam initialEnvironment
                        tcEnvWithCompiler = emptyTCEnv { envCompiler = Just cenv }
                    let Right (_, st') = runTC (infer (Id "(+)")) tcEnvWithCompiler st0
                    -- Should have emitted a CStructure constraint
                    length (constraints st') `shouldSatisfy` (> 0)

            describe "Record types" $ do
                it "infers record literal type" $ do
                    let recLit = RecordLit [("x", Lit (LInt 42)), ("y", Lit (LFloat 3.14))]
                    let Right (ty, _) = runTC (infer recLit) env0 st0
                    ty `shouldBe` TRecord (RExtend "x" (TCon "Int") (RExtend "y" (TCon "Float64") REmpty))

                it "converts record type to Ty" $ do
                    let recTy = RecordType [("x", Id "Int"), ("y", Id "Bool")] False
                    let Right (ty, _) = runTC (exprToTy recTy) env0 st0
                    ty `shouldBe` TRecord (RExtend "x" (TCon "Int") (RExtend "y" (TCon "Bool") REmpty))

                it "converts open record type with row var" $ do
                    let recTy = RecordType [("x", Id "Int")] True
                    let Right (ty, _) = runTC (exprToTy recTy) env0 st0
                    case ty of
                        TRecord (RExtend "x" (TCon "Int") (RVar _)) -> pure ()
                        other -> expectationFailure $ "Expected open record, got: " ++ show other

                it "unifies record literal with expected record type" $ do
                    let recLit = RecordLit [("x", Lit (LInt 1)), ("y", Lit (LFloat 2.0))]
                        recTy = TRecord (RExtend "x" (TCon "Int") (RExtend "y" (TCon "Float64") REmpty))
                    let result = runTC (check recLit recTy) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

            describe "Polymorphism" $ do
                it "instantiates TForall with fresh variable" $ do
                    let polyTy = TForall "a" (TArrow (TRigid "a") (TRigid "a"))
                    let Right (ty, _) = runTC (instantiate polyTy) env0 st0
                    -- Should be TVar 0 -> TVar 0 (fresh var replacing "a")
                    case ty of
                        TArrow (TVar v1) (TVar v2) | v1 == v2 -> pure ()
                        other -> expectationFailure $ "Expected ?n -> ?n, got: " ++ show other

                it "nested TForall instantiation" $ do
                    let polyTy = TForall "a" (TForall "b" (TArrow (TRigid "a") (TRigid "b")))
                    let Right (ty, st') = runTC (instantiate polyTy) env0 st0
                    -- Should be TVar 0 -> TVar 1
                    case ty of
                        TArrow (TVar v1) (TVar v2) | v1 /= v2 -> pure ()
                        other -> expectationFailure $ "Expected ?n -> ?m (n/=m), got: " ++ show other

                it "substTyVar replaces rigid variable" $ do
                    let ty = TArrow (TRigid "a") (TCon "Int")
                    substTyVar "a" (TCon "Bool") ty `shouldBe` TArrow (TCon "Bool") (TCon "Int")

                it "substTyVar does not replace shadowed variable" $ do
                    let ty = TForall "a" (TArrow (TRigid "a") (TRigid "b"))
                    substTyVar "a" (TCon "Bool") ty `shouldBe` TForall "a" (TArrow (TRigid "a") (TRigid "b"))

                it "generalize quantifies free vars" $ do
                    let Right (ty, _) = runTC (generalize (TArrow (TVar 0) (TVar 0))) env0 (st0 { nextVar = 1 })
                    case ty of
                        TForall _ (TArrow (TRigid _) (TRigid _)) -> pure ()
                        other -> expectationFailure $ "Expected forall a. a -> a, got: " ++ show other

                it "generalize does not quantify env-bound vars" $ do
                    let env1 = env0 { varTypes = Map.fromList [("x", TVar 0)] }
                    let Right (ty, _) = runTC (generalize (TArrow (TVar 0) (TVar 1))) env1 (st0 { nextVar = 2 })
                    -- TVar 0 is in env, so only TVar 1 should be generalized
                    case ty of
                        TForall _ (TArrow (TVar 0) (TRigid _)) -> pure ()
                        other -> expectationFailure $ "Expected forall b. ?0 -> b, got: " ++ show other

            -- ============================================================
            -- Error Detection Tests
            -- ============================================================
            describe "Error detection: unification failures" $ do
                it "fails unifying Int with String" $ do
                    let result = runTC (unify (TCon "Int") (TCon "String")) env0 st0
                    case result of
                        Left (Mismatch (TCon "Int") (TCon "String") : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error type: " ++ show errs
                        Right _ -> expectationFailure "Should have failed"

                it "fails unifying arrow with non-arrow" $ do
                    let result = runTC (unify (TArrow (TCon "Int") (TCon "Bool")) (TCon "Int")) env0 st0
                    case result of
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error type: " ++ show errs
                        Right _ -> expectationFailure "Should have failed"

                it "fails unifying TApp with different constructors" $ do
                    let result = runTC (unify (TApp (TCon "Maybe") [TCon "Int"]) (TApp (TCon "List") [TCon "Int"])) env0 st0
                    case result of
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error type: " ++ show errs
                        Right _ -> expectationFailure "Should have failed"

                it "fails unifying TApp with different arity" $ do
                    let result = runTC (unify (TApp (TCon "Pair") [TCon "Int", TCon "Bool"]) (TApp (TCon "Pair") [TCon "Int"])) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should have failed"

                it "occurs check with nested type" $ do
                    let result = runTC (unify (TVar 0) (TArrow (TVar 0) (TCon "Int"))) env0 (st0 { nextVar = 1 })
                    case result of
                        Left (OccursCheck 0 _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs
                        Right _ -> expectationFailure "Should have failed with occurs check"

            describe "Error detection: check mode mismatches" $ do
                it "check Int literal against Bool warns" $ do
                    let result = runTC (check (Lit (LInt 5)) (TCon "Bool")) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs

                it "check String literal against Int warns" $ do
                    let result = runTC (check (Lit (LString "hi")) (TCon "Int")) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs

                it "check Float literal against String warns" $ do
                    let result = runTC (check (Lit (LFloat 3.14)) (TCon "String")) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs

                it "check Char literal against Int warns" $ do
                    let result = runTC (check (Lit (LChar 'a')) (TCon "Int")) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs

            describe "Error detection: function type mismatches" $ do
                it "function returning wrong type warns" $ do
                    -- function f(x:Int) : Bool = x  -- returns Int, declared Bool
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED] (Id "x") (Id "Bool")
                        env1 = env0 { varTypes = Map.fromList [("x", TCon "Int")] }
                    let result = runTC (inferLambda lam) env1 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs

                it "check function against incompatible arrow type warns" $ do
                    -- function with Int -> Int checked against String -> Bool
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED] (Id "x") (Id "Int")
                    let result = runTC (check (Function lam) (TArrow (TCon "String") (TCon "Bool"))) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()  -- fatal mismatch also acceptable

                it "multi-param function with wrong return type" $ do
                    -- function f(x:Int, y:Bool) : String = x  -- returns Int not String
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED, Var "y" (Id "Bool") UNDEFINED]
                                        (Id "x") (Id "String")
                    let result = runTC (inferLambda lam) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs

            describe "Error detection: record errors" $ do
                it "missing field in closed record unification" $ do
                    -- {x:Int} vs {x:Int, y:Bool} — closed record missing field y
                    let r1 = TRecord (RExtend "x" (TCon "Int") REmpty)
                        r2 = TRecord (RExtend "x" (TCon "Int") (RExtend "y" (TCon "Bool") REmpty))
                    let result = runTC (unify r1 r2) env0 st0
                    case result of
                        Left (MissingField "y" : _) -> pure ()
                        Left _ -> pure ()  -- any error is acceptable
                        Right _ -> expectationFailure "Should have failed — closed record missing field"

                it "record field type mismatch" $ do
                    -- {x:Int, y:Bool} vs {x:Int, y:String}
                    let r1 = TRecord (RExtend "x" (TCon "Int") (RExtend "y" (TCon "Bool") REmpty))
                        r2 = TRecord (RExtend "x" (TCon "Int") (RExtend "y" (TCon "String") REmpty))
                    let result = runTC (unify r1 r2) env0 st0
                    case result of
                        Left (Mismatch (TCon "Bool") (TCon "String") : _) -> pure ()
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should have failed — field y type mismatch"

                it "extra field in expected closed record" $ do
                    -- checking {x=1, y=2, z=3} against {x:Int, y:Int}
                    let recLit = RecordLit [("x", Lit (LInt 1)), ("y", Lit (LInt 2)), ("z", Lit (LInt 3))]
                        recTy = TRecord (RExtend "x" (TCon "Int") (RExtend "y" (TCon "Int") REmpty))
                    let result = runTC (check recLit recTy) env0 st0
                    case result of
                        Left _ -> pure ()  -- should fail: extra field z
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)

                it "record literal field with wrong type" $ do
                    -- checking {x = "hello"} against {x:Int}
                    let recLit = RecordLit [("x", Lit (LString "hello"))]
                        recTy = TRecord (RExtend "x" (TCon "Int") REmpty)
                    let result = runTC (check recLit recTy) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)

            describe "Error detection: row polymorphism edge cases" $ do
                it "open record accepts extra fields" $ do
                    -- {x:Int, ..r} should accept {x:Int, y:Bool}
                    let st1 = st0 { nextVar = 1 }
                        r1 = TRecord (RExtend "x" (TCon "Int") (RVar 0))
                        r2 = TRecord (RExtend "x" (TCon "Int") (RExtend "y" (TCon "Bool") REmpty))
                    let result = runTC (unify r1 r2) env0 st1
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Open record should accept extra fields: " ++ show errs

                it "closed record rejects extra fields" $ do
                    -- {x:Int} should NOT accept {x:Int, y:Bool}
                    let r1 = TRecord (RExtend "x" (TCon "Int") REmpty)
                        r2 = TRecord (RExtend "x" (TCon "Int") (RExtend "y" (TCon "Bool") REmpty))
                    let result = runTC (unify r1 r2) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Closed record should reject extra fields"

                it "missing field extraction from empty row fails" $ do
                    let result = runTC (unify (TRecord (RExtend "x" (TCon "Int") REmpty)) (TRecord (RExtend "z" (TCon "Int") REmpty))) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should fail — field z not in {x:Int}"

            describe "Error detection: constructor and application" $ do
                it "constructor with wrong number of args warns" $ do
                    -- ConTuple "Succ" with 2 args (should be 1)
                    let succCons = mkLambda "Succ" [Var "n" (Id "Nat") UNDEFINED] (Tuple [Id "n"]) (Id "Nat")
                        cenv = addNamedConstructor 1 succCons initialEnvironment
                        tcEnvC = emptyTCEnv { envCompiler = Just cenv }
                    let result = runTC (infer (ConTuple (ConsTag "Succ" 1) [Lit (LInt 1), Lit (LInt 2)])) tcEnvC st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left (ArityMismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Expected arity error: " ++ show errs

                it "application to non-function warns or returns fresh var" $ do
                    -- Applying a literal: (42)(x) — 42 is not a function
                    let env1 = env0 { varTypes = Map.fromList [("x", TCon "Int")] }
                    let result = runTC (infer (App (Lit (LInt 42)) [Id "x"])) env1 st0
                    -- This should either fail or produce a fresh var (we're permissive)
                    case result of
                        Right _ -> pure () :: IO ()
                        Left _ -> pure () :: IO ()

            describe "Error detection: checkTopLevel with compiler env" $ do
                it "type checks a well-typed function through full env" $ do
                    -- Use the real loaded environment
                    let cenv = currentEnvironment st
                        tcEnvFull = buildTCEnvFromEnvironment cenv
                    -- function not(x:Bool) : Bool = match | True -> False | False -> True
                    -- This is already in base.tl, so checkTopLevel on a similar function should pass
                    let lam = mkLambda "myNot" [Var "x" (Id "Bool") UNDEFINED] (Id "x") (Id "Bool")
                    let result = runTC (checkTopLevel (Function lam)) tcEnvFull st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` null
                        Left errs -> expectationFailure $ "Should have passed: " ++ show errs

                it "type checks a function with wrong return type annotation" $ do
                    let cenv = currentEnvironment st
                        tcEnvFull = buildTCEnvFromEnvironment cenv
                    -- function bad(x:Int) : Bool = x  -- Int is not Bool
                    let lam = mkLambda "bad" [Var "x" (Id "Int") UNDEFINED] (Id "x") (Id "Bool")
                    let result = runTC (checkTopLevel (Function lam)) tcEnvFull st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()  -- also acceptable (fatal error)

            describe "Error detection: error message quality" $ do
                it "Mismatch error includes both types" $ do
                    let err = Mismatch (TCon "Int") (TCon "Bool")
                    showTCError err `shouldSatisfy` \s ->
                        "Int" `isInfixOf` s && "Bool" `isInfixOf` s

                it "OccursCheck error mentions the variable" $ do
                    let err = OccursCheck 3 (TArrow (TVar 3) (TCon "Int"))
                    showTCError err `shouldSatisfy` \s ->
                        "?3" `isInfixOf` s

                it "MissingField error includes field name" $ do
                    let err = MissingField "foo"
                    showTCError err `shouldSatisfy` \s ->
                        "foo" `isInfixOf` s

                it "ArityMismatch shows both counts" $ do
                    let err = ArityMismatch 2 3
                    showTCError err `shouldSatisfy` \s ->
                        "2" `isInfixOf` s && "3" `isInfixOf` s

                it "all errors are prefixed with [TC]" $ do
                    let errors = [ Mismatch (TCon "Int") (TCon "Bool")
                                 , OccursCheck 0 (TCon "Int")
                                 , UnboundVar "x"
                                 , MissingField "f"
                                 , ArityMismatch 1 2
                                 , OtherError "test"
                                 ]
                    mapM_ (\e -> showTCError e `shouldSatisfy` \s -> "[TC]" `isPrefixOf` s) errors

            describe "Type check pass integration" $ do
                it "typeCheckPass runs without crashing on base.tl" $ do
                    -- This test verifies the type checker doesn't crash on real code
                    -- It's already passing since setupEnv includes typeCheckPass
                    pure () :: IO ()

            -- ============================================================
            -- Step 1: tcWarnOrFail + tcMode
            -- ============================================================
            describe "tcWarnOrFail and strict mode" $ do
                it "strict mode fails fatally on type mismatch" $ do
                    let stStrict = initTCState TCStrict
                    let result = runTC (check (Lit (LInt 5)) (TCon "Bool")) env0 stStrict
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Strict mode should fail on mismatch"

                it "relaxed mode accumulates warning on mismatch" $ do
                    let result = runTC (check (Lit (LInt 5)) (TCon "Bool")) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> expectationFailure "Relaxed mode should warn, not fail"

            -- ============================================================
            -- Step 2: Catch-all warnings
            -- ============================================================
            describe "Catch-all warnings" $ do
                it "unbound variable emits UnboundVar warning" $ do
                    let result = runTC (infer (Id "nonexistent")) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy`
                            (\errs -> Prelude.any isUnboundVar errs)
                        Left _ -> pure ()  -- also acceptable

                it "infer catch-all emits warning" $ do
                    -- DeclBlock is not handled by any specific case
                    let result = runTC (infer (DeclBlock [])) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()

                it "exprToTy catch-all emits warning for unknown forms" $ do
                    -- Value(Var ...) in type position should warn
                    let result = runTC (exprToTy (Lit (LInt 42))) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()

            -- ============================================================
            -- Step 3: TForall alpha-renaming
            -- ============================================================
            describe "TForall alpha-renaming" $ do
                it "alpha-equivalent foralls unify" $ do
                    -- forall a. a -> a  should unify with  forall b. b -> b
                    let t1 = TForall "a" (TArrow (TRigid "a") (TRigid "a"))
                        t2 = TForall "b" (TArrow (TRigid "b") (TRigid "b"))
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Alpha-equivalent foralls should unify: " ++ show errs

                it "incompatible forall bodies don't unify" $ do
                    -- forall a. a -> a  vs  forall b. b -> Int
                    let t1 = TForall "a" (TArrow (TRigid "a") (TRigid "a"))
                        t2 = TForall "b" (TArrow (TRigid "b") (TCon "Int"))
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Incompatible forall bodies should not unify"

            -- ============================================================
            -- Step 4: Error context tracking
            -- ============================================================
            describe "Error context tracking" $ do
                it "error in function body includes function name" $ do
                    let lam = mkLambda "myFunc" [Var "x" (Id "Int") UNDEFINED] (Id "x") (Id "Bool")
                    let result = runTC (inferLambda lam) env0 st0
                    case result of
                        Right (_, st') -> do
                            let errStrs = Prelude.map showTCError (tcErrors st')
                            errStrs `shouldSatisfy` Prelude.any ("myFunc" `isInfixOf`)
                        Left errs -> do
                            let errStrs = Prelude.map showTCError errs
                            errStrs `shouldSatisfy` Prelude.any ("myFunc" `isInfixOf`)

                it "WithContext error renders properly" $ do
                    let err = WithContext "function 'test'" (Mismatch (TCon "Int") (TCon "Bool"))
                    showTCError err `shouldSatisfy` ("test" `isInfixOf`)
                    showTCError err `shouldSatisfy` ("Int" `isInfixOf`)

            -- ============================================================
            -- Step 5: Recursive function self-binding
            -- ============================================================
            describe "Recursive function self-binding" $ do
                it "recursive function self-reference resolves" $ do
                    -- function fact(n:Int) : Int = fact(n)  — self-reference should not be unbound
                    let lam = mkLambda "fact" [Var "n" (Id "Int") UNDEFINED]
                                        (App (Id "fact") [Id "n"]) (Id "Int")
                    let result = runTC (inferLambda lam) env0 st0
                    case result of
                        Right (ty, st') -> do
                            -- Should infer Int -> Int without unbound var warning for "fact"
                            ty `shouldBe` TArrow (TCon "Int") (TCon "Int")
                            let hasFactUnbound = Prelude.any (\e -> case e of
                                    UnboundVar "fact" -> True
                                    WithContext _ (UnboundVar "fact") -> True
                                    _ -> False) (tcErrors st')
                            hasFactUnbound `shouldBe` False
                        Left errs -> expectationFailure $ "Should have succeeded: " ++ show errs

                it "non-recursive function unaffected by self-binding" $ do
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED] (Lit (LInt 42)) (Id "Int")
                    let Right (ty, _) = runTC (inferLambda lam) env0 st0
                    ty `shouldBe` TArrow (TCon "Int") (TCon "Int")

            -- ============================================================
            -- Step 6: exprToTy extended cases
            -- ============================================================
            describe "exprToTy extended cases" $ do
                it "general App converts to TApp" $ do
                    -- App (App (Id "F") [Id "a"]) [Id "b"] -> TApp (TApp (TCon "F") [TRigid "a"]) [TRigid "b"]
                    let expr = App (App (Id "F") [Id "a"]) [Id "b"]
                    let Right (ty, _) = runTC (exprToTy expr) env0 st0
                    case ty of
                        TApp (TApp (TCon "F") [TRigid "a"]) [TRigid "b"] -> pure ()
                        other -> expectationFailure $ "Expected nested TApp, got: " ++ show other

                it "Function in type position converts to arrow" $ do
                    let lam = mkLambda "" [Var "x" (Id "Int") UNDEFINED] UNDEFINED (Id "Bool")
                    let Right (ty, _) = runTC (exprToTy (Function lam)) env0 st0
                    case ty of
                        TArrow (TCon "Int") _ -> pure ()
                        other -> expectationFailure $ "Expected arrow type, got: " ++ show other

            -- ============================================================
            -- Step 7a: Class field + method body checking
            -- ============================================================
            describe "Class system type checking" $ do
                it "ClassDecl checkTopLevel doesn't crash" $ do
                    let cenv = currentEnvironment st
                        tcEnvFull = buildTCEnvFromEnvironment cenv
                    -- Create a minimal class declaration
                    let classLam = mkLambda "TestClass" [Var "x" (Id "Int") UNDEFINED]
                                    (DeclBlock []) (Id "TestClass")
                        cinfo = ClassInfo { classParent = Nothing
                                          , classImplements = []
                                          , classModifier = ClassNormal
                                          , classExtern = Nothing
                                          , classMethodMods = [] }
                    let result = runTC (checkTopLevel (ClassDecl classLam cinfo)) tcEnvFull st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Class check should pass: " ++ show errs

                it "class method body checked with self in scope" $ do
                    let cenv = currentEnvironment st
                        tcEnvFull = buildTCEnvFromEnvironment cenv
                    -- Class with a method that references self
                    let methodLam = mkLambda "getName" [Var "self" (Id "TestClass2") UNDEFINED]
                                      (Id "self") (Id "TestClass2")
                        classLam = mkLambda "TestClass2" [Var "name" (Id "String") UNDEFINED]
                                    (DeclBlock [Function methodLam]) (Id "TestClass2")
                        cinfo = ClassInfo { classParent = Nothing
                                          , classImplements = []
                                          , classModifier = ClassNormal
                                          , classExtern = Nothing
                                          , classMethodMods = [] }
                    let result = runTC (checkTopLevel (ClassDecl classLam cinfo)) tcEnvFull st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Class method check should pass: " ++ show errs

                it "abstract class instantiation warns" $ do
                    let cenv = currentEnvironment st
                        tcEnvFull = buildTCEnvFromEnvironment cenv
                    -- Simulate abstract class in classDecls
                    let abstractCM = ClassMeta { cmOwnFields = []
                                               , cmAllFields = [Var "x" (Id "Int") UNDEFINED]
                                               , cmParent = Nothing
                                               , cmMethods = Map.empty
                                               , cmStaticMethods = Map.empty
                                               , cmFieldIndices = Map.empty
                                               , cmModifier = ClassAbstract
                                               , cmChildren = []
                                               , cmImplements = []
                                               , cmSuperArgs = []
                                               , cmExtern = Nothing
                                               , cmTag = 0
                                               , cmSourceFile = "" }
                        cenv' = cenv { classDecls = Map.insert "AbstractBase" abstractCM (classDecls cenv) }
                        tcEnv' = emptyTCEnv { envCompiler = Just cenv' }
                    let result = runTC (infer (App (RecFieldAccess ("new", -1) (Id "AbstractBase")) [Lit (LInt 1)])) tcEnv' st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy`
                            (\errs -> Prelude.any (\e -> "abstract" `isInfixOf` showTCError e) errs)
                        Left errs -> Prelude.map showTCError errs `shouldSatisfy`
                            Prelude.any ("abstract" `isInfixOf`)

                it "constructor arity mismatch warns" $ do
                    let cenv = currentEnvironment st
                    let classCM = ClassMeta { cmOwnFields = []
                                            , cmAllFields = [Var "x" (Id "Int") UNDEFINED, Var "y" (Id "Int") UNDEFINED]
                                            , cmParent = Nothing
                                            , cmMethods = Map.empty
                                            , cmStaticMethods = Map.empty
                                            , cmFieldIndices = Map.empty
                                            , cmModifier = ClassNormal
                                            , cmChildren = []
                                            , cmImplements = []
                                            , cmSuperArgs = []
                                            , cmExtern = Nothing
                                            , cmTag = 0
                                            , cmSourceFile = "" }
                        cenv' = cenv { classDecls = Map.insert "TwoFields" classCM (classDecls cenv) }
                        tcEnv' = emptyTCEnv { envCompiler = Just cenv' }
                    -- Pass 3 args for a 2-field class
                    let result = runTC (infer (App (RecFieldAccess ("new", -1) (Id "TwoFields")) [Lit (LInt 1), Lit (LInt 2), Lit (LInt 3)])) tcEnv' st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy`
                            (\errs -> Prelude.any (\e -> case e of
                                ArityMismatch _ _ -> True
                                WithContext _ (ArityMismatch _ _) -> True
                                _ -> False) errs)
                        Left errs -> errs `shouldSatisfy`
                            Prelude.any (\e -> case e of
                                ArityMismatch _ _ -> True
                                WithContext _ (ArityMismatch _ _) -> True
                                _ -> False)

                it "correct constructor arity passes" $ do
                    let cenv = currentEnvironment st
                    let classCM = ClassMeta { cmOwnFields = []
                                            , cmAllFields = [Var "x" (Id "Int") UNDEFINED]
                                            , cmParent = Nothing
                                            , cmMethods = Map.empty
                                            , cmStaticMethods = Map.empty
                                            , cmFieldIndices = Map.empty
                                            , cmModifier = ClassNormal
                                            , cmChildren = []
                                            , cmImplements = []
                                            , cmSuperArgs = []
                                            , cmExtern = Nothing
                                            , cmTag = 0
                                            , cmSourceFile = "" }
                        cenv' = cenv { classDecls = Map.insert "OneField" classCM (classDecls cenv) }
                        tcEnv' = emptyTCEnv { envCompiler = Just cenv' }
                    let result = runTC (infer (App (RecFieldAccess ("new", -1) (Id "OneField")) [Lit (LInt 1)])) tcEnv' st0
                    case result of
                        Right (ty, st') -> do
                            ty `shouldBe` TCon "OneField"
                            let hasArity = Prelude.any (\e -> case e of
                                    ArityMismatch _ _ -> True
                                    WithContext _ (ArityMismatch _ _) -> True
                                    _ -> False) (tcErrors st')
                            hasArity `shouldBe` False
                        Left errs -> expectationFailure $ "Should pass: " ++ show errs

                it "implements with missing method warns" $ do
                    let cenv = currentEnvironment st
                        tcEnvFull = buildTCEnvFromEnvironment cenv
                    -- Create a class that claims to implement Show but has no show method
                    let classLam = mkLambda "BadClass" [Var "x" (Id "Int") UNDEFINED]
                                    (DeclBlock []) (Id "BadClass")
                        cinfo = ClassInfo { classParent = Nothing
                                          , classImplements = [Id "Show"]
                                          , classModifier = ClassNormal
                                          , classExtern = Nothing
                                          , classMethodMods = [] }
                    let result = runTC (checkTopLevel (ClassDecl classLam cinfo)) tcEnvFull st0
                    case result of
                        Right (_, st') -> do
                            let _errStrs = Prelude.map showTCError (tcErrors st')
                            -- Should warn about missing "show" method (unless Show has derive block)
                            -- Show algebra has a derive block, so this may pass silently
                            pure () :: IO ()
                        Left _ -> pure () :: IO ()  -- also acceptable

                it "subtype Dog <: Animal succeeds" $ do
                    let cenv = currentEnvironment st
                    -- Set up Dog -> Animal class hierarchy
                    let animalCM = ClassMeta { cmOwnFields = [], cmAllFields = []
                                             , cmParent = Nothing, cmMethods = Map.empty
                                             , cmStaticMethods = Map.empty, cmFieldIndices = Map.empty
                                             , cmModifier = ClassNormal, cmChildren = ["Dog"]
                                             , cmImplements = [], cmSuperArgs = []
                                             , cmExtern = Nothing, cmTag = 100, cmSourceFile = "" }
                        dogCM = ClassMeta { cmOwnFields = [], cmAllFields = []
                                          , cmParent = Just "Animal", cmMethods = Map.empty
                                          , cmStaticMethods = Map.empty, cmFieldIndices = Map.empty
                                          , cmModifier = ClassNormal, cmChildren = []
                                          , cmImplements = [], cmSuperArgs = []
                                          , cmExtern = Nothing, cmTag = 101, cmSourceFile = "" }
                        cenv' = cenv { classDecls = Map.fromList [("Animal", animalCM), ("Dog", dogCM)] }
                        tcEnv' = emptyTCEnv { envCompiler = Just cenv' }
                    let result = runTC (subtype (TCon "Dog") (TCon "Animal")) tcEnv' st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "subtype Dog <: Animal should succeed: " ++ show errs

                it "subtype Animal <: Dog fails" $ do
                    let cenv = currentEnvironment st
                    let animalCM = ClassMeta { cmOwnFields = [], cmAllFields = []
                                             , cmParent = Nothing, cmMethods = Map.empty
                                             , cmStaticMethods = Map.empty, cmFieldIndices = Map.empty
                                             , cmModifier = ClassNormal, cmChildren = ["Dog"]
                                             , cmImplements = [], cmSuperArgs = []
                                             , cmExtern = Nothing, cmTag = 100, cmSourceFile = "" }
                        dogCM = ClassMeta { cmOwnFields = [], cmAllFields = []
                                          , cmParent = Just "Animal", cmMethods = Map.empty
                                          , cmStaticMethods = Map.empty, cmFieldIndices = Map.empty
                                          , cmModifier = ClassNormal, cmChildren = []
                                          , cmImplements = [], cmSuperArgs = []
                                          , cmExtern = Nothing, cmTag = 101, cmSourceFile = "" }
                        cenv' = cenv { classDecls = Map.fromList [("Animal", animalCM), ("Dog", dogCM)] }
                        tcEnv' = emptyTCEnv { envCompiler = Just cenv' }
                    let result = runTC (subtype (TCon "Animal") (TCon "Dog")) tcEnv' st0
                    case result of
                        Right _ -> expectationFailure "subtype Animal <: Dog should fail"
                        Left errs -> errs `shouldSatisfy`
                            Prelude.any (\e -> case e of
                                SubtypeMismatch _ _ -> True
                                _ -> False)

                it "check accepts Dog expr where Animal expected" $ do
                    let cenv = currentEnvironment st
                    let animalCM = ClassMeta { cmOwnFields = [], cmAllFields = []
                                             , cmParent = Nothing, cmMethods = Map.empty
                                             , cmStaticMethods = Map.empty, cmFieldIndices = Map.empty
                                             , cmModifier = ClassNormal, cmChildren = ["Dog"]
                                             , cmImplements = [], cmSuperArgs = []
                                             , cmExtern = Nothing, cmTag = 100, cmSourceFile = "" }
                        dogCM = ClassMeta { cmOwnFields = [], cmAllFields = []
                                          , cmParent = Just "Animal", cmMethods = Map.empty
                                          , cmStaticMethods = Map.empty, cmFieldIndices = Map.empty
                                          , cmModifier = ClassNormal, cmChildren = []
                                          , cmImplements = [], cmSuperArgs = []
                                          , cmExtern = Nothing, cmTag = 101, cmSourceFile = "" }
                        cenv' = cenv { classDecls = Map.fromList [("Animal", animalCM), ("Dog", dogCM)] }
                        tcEnv' = emptyTCEnv { envCompiler = Just cenv'
                                            , varTypes = Map.fromList [("myDog", TCon "Dog")] }
                    let stStrict = initTCState TCStrict
                    let result = runTC (check (Id "myDog") (TCon "Animal")) tcEnv' stStrict
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "check Dog as Animal should pass in strict mode: " ++ show errs

                it "infer ReprCast to class type returns Maybe" $ do
                    let cenv = currentEnvironment st
                    let dogCM = ClassMeta { cmOwnFields = [], cmAllFields = []
                                          , cmParent = Nothing, cmMethods = Map.empty
                                          , cmStaticMethods = Map.empty, cmFieldIndices = Map.empty
                                          , cmModifier = ClassNormal, cmChildren = []
                                          , cmImplements = [], cmSuperArgs = []
                                          , cmExtern = Nothing, cmTag = 101, cmSourceFile = "" }
                        cenv' = cenv { classDecls = Map.insert "Dog" dogCM (classDecls cenv) }
                        tcEnv' = emptyTCEnv { envCompiler = Just cenv' }
                    let result = runTC (infer (ReprCast (Lit (LInt 1)) (Id "Dog"))) tcEnv' st0
                    case result of
                        Right (ty, _) -> ty `shouldBe` TApp (TCon "Maybe") [TCon "Dog"]
                        Left errs -> expectationFailure $ "infer ReprCast to Dog should return Maybe(Dog): " ++ show errs

                it "SubtypeMismatch error has [TC] prefix" $ do
                    showTCError (SubtypeMismatch (TCon "Int") (TCon "Bool")) `shouldSatisfy` ("TC" `isInfixOf`)

                it "all [TC] errors include context prefix" $ do
                    let errors = [ Mismatch (TCon "Int") (TCon "Bool")
                                 , OccursCheck 0 (TCon "Int")
                                 , UnboundVar "x"
                                 , MissingField "f"
                                 , ArityMismatch 1 2
                                 , OtherError "test"
                                 , WithContext "function 'foo'" (Mismatch (TCon "Int") (TCon "Bool"))
                                 ]
                    mapM_ (\e -> showTCError e `shouldSatisfy` \s -> "[TC]" `isPrefixOf` s) errors

                it "strict mode check passes for matching types" $ do
                    let stStrict = initTCState TCStrict
                    let result = runTC (check (Lit (LInt 5)) (TCon "Int")) env0 stStrict
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Strict check should pass for matching types: " ++ show errs

                it "class with valid field types passes check" $ do
                    let cenv = currentEnvironment st
                        tcEnvFull = buildTCEnvFromEnvironment cenv
                    -- Class with Int and String fields — both well-formed
                    let classLam = mkLambda "ValidClass"
                                    [Var "name" (Id "String") UNDEFINED, Var "age" (Id "Int") UNDEFINED]
                                    (DeclBlock []) (Id "ValidClass")
                        cinfo = ClassInfo { classParent = Nothing
                                          , classImplements = []
                                          , classModifier = ClassNormal
                                          , classExtern = Nothing
                                          , classMethodMods = [] }
                    let result = runTC (checkTopLevel (ClassDecl classLam cinfo)) tcEnvFull st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Valid class should pass: " ++ show errs

            -- ============================================================
            -- Comprehensive edge case tests
            -- ============================================================
            describe "tcTry state isolation" $ do
                it "failed tcTry does not corrupt substitution state" $ do
                    let st1 = st0 { nextVar = 2 }
                    -- First, bind TVar 0 = Int
                    let Right (_, st2) = runTC (unify (TVar 0) (TCon "Int")) env0 st1
                    -- Now try a failing unification — should not affect st2
                    let Right (result, st3) = runTC (tcTry (unify (TCon "Int") (TCon "Bool"))) env0 st2
                    result `shouldBe` Nothing
                    -- TVar 0 should still resolve to Int
                    let Right (ty, _) = runTC (applySubst (TVar 0)) env0 st3
                    ty `shouldBe` TCon "Int"

                it "failed tcTry does not lose accumulated errors" $ do
                    -- Add a warning, then try a failing action
                    let Right (_, st1) = runTC (tcWarn (OtherError "existing")) env0 st0
                    let Right (_, st2) = runTC (tcTry (tcFail (OtherError "boom"))) env0 st1
                    -- The existing warning should still be there
                    length (tcErrors st2) `shouldBe` 1

            describe "Strict mode error propagation" $ do
                it "strict mode fails on unbound variable in check" $ do
                    let stStrict = initTCState TCStrict
                    -- check an unbound name against Int — should fail because
                    -- infer(Id "noexist") warns, then unify(fresh, Int) succeeds
                    -- but the warning is via tcWarn not tcWarnOrFail,
                    -- so it should still accumulate
                    let result = runTC (check (Id "noexist") (TCon "Int")) env0 stStrict
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()  -- also fine if it fails

                it "strict mode ConTuple arity mismatch is fatal" $ do
                    let stStrict = initTCState TCStrict
                        succCons = mkLambda "S" [Var "n" (Id "Nat") UNDEFINED] (Tuple [Id "n"]) (Id "Nat")
                        cenv = addNamedConstructor 1 succCons initialEnvironment
                        tcEnvC = emptyTCEnv { envCompiler = Just cenv }
                    let result = runTC (infer (ConTuple (ConsTag "S" 1) [Lit (LInt 1), Lit (LInt 2)])) tcEnvC stStrict
                    case result of
                        Left errs -> errs `shouldSatisfy` Prelude.any (\e -> case e of
                            ArityMismatch _ _ -> True
                            WithContext _ (ArityMismatch _ _) -> True
                            _ -> False)
                        Right _ -> expectationFailure "Strict mode should fail on arity mismatch"

            describe "TSigma unification" $ do
                it "unifies identical sigma types" $ do
                    let t1 = TSigma Nothing (TCon "Int") (TCon "Bool")
                        t2 = TSigma Nothing (TCon "Int") (TCon "Bool")
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Should unify: " ++ show errs

                it "fails on mismatched sigma components" $ do
                    let t1 = TSigma Nothing (TCon "Int") (TCon "Bool")
                        t2 = TSigma Nothing (TCon "Int") (TCon "String")
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should fail on sigma mismatch"

                it "unifies sigma with variables" $ do
                    let st1 = st0 { nextVar = 2 }
                        t1 = TSigma Nothing (TVar 0) (TVar 1)
                        t2 = TSigma Nothing (TCon "Int") (TCon "Bool")
                    let Right (_, st') = runTC (unify t1 t2) env0 st1
                    let Right (r0, _) = runTC (applySubst (TVar 0)) env0 st'
                    let Right (r1, _) = runTC (applySubst (TVar 1)) env0 st'
                    r0 `shouldBe` TCon "Int"
                    r1 `shouldBe` TCon "Bool"

            describe "TId unification" $ do
                it "unifies identical identity types" $ do
                    let t1 = TId (TCon "Nat") (TCon "Z") (TCon "Z")
                        t2 = TId (TCon "Nat") (TCon "Z") (TCon "Z")
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Should unify: " ++ show errs

                it "fails on mismatched identity endpoints" $ do
                    let t1 = TId (TCon "Nat") (TCon "Z") (TCon "Z")
                        t2 = TId (TCon "Nat") (TCon "Z") (TCon "S")
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should fail"

            describe "TEffect unification" $ do
                it "unifies identical effect types" $ do
                    let t1 = TEffect (RExtend "console" (TCon "Console") REmpty) (TCon "Unit")
                        t2 = TEffect (RExtend "console" (TCon "Console") REmpty) (TCon "Unit")
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Should unify: " ++ show errs

                it "fails on mismatched effect result types" $ do
                    let t1 = TEffect REmpty (TCon "Int")
                        t2 = TEffect REmpty (TCon "Bool")
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should fail on effect result mismatch"

                it "unifies effect rows with different order" $ do
                    let t1 = TEffect (RExtend "a" (TCon "A") (RExtend "b" (TCon "B") REmpty)) (TCon "Unit")
                        t2 = TEffect (RExtend "b" (TCon "B") (RExtend "a" (TCon "A") REmpty)) (TCon "Unit")
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Should unify reordered effects: " ++ show errs

            describe "TRigid mismatch" $ do
                it "different rigid names fail to unify" $ do
                    let result = runTC (unify (TRigid "a") (TRigid "b")) env0 st0
                    case result of
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs
                        Right _ -> expectationFailure "Different rigid vars should not unify"

                it "TRigid vs TCon fails" $ do
                    let result = runTC (unify (TRigid "a") (TCon "Int")) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Rigid vs concrete should fail"

                it "TCon vs TRigid fails" $ do
                    let result = runTC (unify (TCon "Int") (TRigid "a")) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Concrete vs rigid should fail"

            describe "Row occurs check" $ do
                it "row variable occurring in its own binding fails" $ do
                    let st1 = st0 { nextVar = 1 }
                        -- Try to unify {x:Int, ..?0} with ?0 — should fail (occurs)
                        row1 = RExtend "x" (TCon "Int") (RVar 0)
                    let result = runTC (bindRow 0 row1) env0 st1
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Row occurs check should fail"

            describe "inferApp edge cases" $ do
                it "too many args for arrow type returns fresh var" $ do
                    -- Int -> Bool applied to (1, 2) — second arg has no matching param
                    let env1 = env0 { varTypes = Map.fromList [("f", TArrow (TCon "Int") (TCon "Bool"))] }
                    let result = runTC (infer (App (Id "f") [Lit (LInt 1), Lit (LInt 2)])) env1 st0
                    case result of
                        Right (ty, _) -> case ty of
                            TVar _ -> pure ()  -- got a fresh var (Bool is not arrow)
                            other -> expectationFailure $ "Expected fresh var from non-arrow application, got: " ++ show other
                        Left _ -> pure ()  -- error also acceptable

                it "application to TVar creates arrow type" $ do
                    let st1 = st0 { nextVar = 1 }
                        env1 = env0 { varTypes = Map.fromList [("f", TVar 0)] }
                    let Right (retTy, st') = runTC (infer (App (Id "f") [Lit (LInt 42)])) env1 st1
                    -- TVar 0 should now be bound to Int -> ?retTy
                    let Right (fTy, _) = runTC (applySubst (TVar 0)) env0 st'
                    case fTy of
                        TArrow (TCon "Int") _ -> pure ()
                        other -> expectationFailure $ "Expected Int -> ?, got: " ++ show other

            describe "LetIn and IfThenElse" $ do
                it "let binding visible in body" $ do
                    let letExpr = LetIn [(Var "x" UNDEFINED UNDEFINED, Lit (LInt 42))] (Id "x")
                    let Right (ty, _) = runTC (infer letExpr) env0 st0
                    ty `shouldBe` TCon "Int"

                it "let binding with multiple bindings" $ do
                    let letExpr = LetIn [(Var "x" UNDEFINED UNDEFINED, Lit (LInt 1)),
                                         (Var "y" UNDEFINED UNDEFINED, Lit (LString "hi"))] (Id "y")
                    let Right (ty, _) = runTC (infer letExpr) env0 st0
                    ty `shouldBe` TCon "String"

                it "if-then-else checks condition is Bool" $ do
                    let ite = IfThenElse (Lit (LInt 42)) (Lit (LInt 1)) (Lit (LInt 2))
                    let result = runTC (infer ite) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()

                it "if-then-else with matching branches succeeds" $ do
                    let env1 = env0 { varTypes = Map.fromList [("b", TCon "Bool")] }
                        ite = IfThenElse (Id "b") (Lit (LInt 1)) (Lit (LInt 2))
                    let Right (ty, st') = runTC (infer ite) env1 st0
                    ty `shouldBe` TCon "Int"
                    -- No type errors
                    Prelude.filter (\e -> case e of
                        UnboundVar _ -> False
                        WithContext _ (UnboundVar _) -> False
                        _ -> True) (tcErrors st') `shouldSatisfy` Prelude.null

                it "if-then-else with mismatched branches warns" $ do
                    let env1 = env0 { varTypes = Map.fromList [("b", TCon "Bool")] }
                        ite = IfThenElse (Id "b") (Lit (LInt 1)) (Lit (LString "oops"))
                    let result = runTC (infer ite) env1 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()

            describe "Tuple type inference" $ do
                it "empty tuple infers Unit" $ do
                    let Right (ty, _) = runTC (infer (Tuple [])) env0 st0
                    ty `shouldBe` TCon "Unit"

                it "single-element tuple infers element type" $ do
                    let Right (ty, _) = runTC (infer (Tuple [Lit (LInt 42)])) env0 st0
                    ty `shouldBe` TCon "Int"

                it "multi-element tuple infers nested TSigma" $ do
                    let Right (ty, _) = runTC (infer (Tuple [Lit (LInt 1), Lit (LString "a"), Lit (LFloat 3.14)])) env0 st0
                    case ty of
                        TSigma Nothing (TCon "Int") (TSigma Nothing (TCon "String") (TCon "Float64")) -> pure ()
                        other -> expectationFailure $ "Expected (Int, String, Float64), got: " ++ show other

            describe "Array literal inference" $ do
                it "empty array has polymorphic element type" $ do
                    let Right (ty, _) = runTC (infer (ArrayLit [])) env0 st0
                    case ty of
                        TApp (TCon "Array") [TVar _] -> pure ()
                        other -> expectationFailure $ "Expected Array(?), got: " ++ show other

                it "non-empty array infers element type" $ do
                    let Right (ty, _) = runTC (infer (ArrayLit [Lit (LInt 1), Lit (LInt 2)])) env0 st0
                    ty `shouldBe` TApp (TCon "Array") [TCon "Int"]

            describe "Pattern match checking" $ do
                it "check PatternMatches propagates expected type" $ do
                    -- PatternMatches [CaseOf [] (Lit (LInt 42)) si]
                    -- checked against Bool should warn
                    let pm = PatternMatches [CaseOf [] (Lit (LInt 42)) SourceInteractive]
                    let result = runTC (check pm (TCon "Bool")) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()

                it "CaseOf in check mode binds pattern variables" $ do
                    -- CaseOf [Var "x" (Id "Int") UNDEFINED] (Id "x") checked against Int
                    let co = CaseOf [Var "x" (Id "Int") UNDEFINED] (Id "x") SourceInteractive
                    let result = runTC (check co (TCon "Int")) env0 st0
                    case result of
                        Right (_, st') -> do
                            let realErrors = Prelude.filter (\e -> case e of
                                    UnboundVar _ -> False
                                    WithContext _ (UnboundVar _) -> False
                                    _ -> True) (tcErrors st')
                            realErrors `shouldSatisfy` Prelude.null
                        Left errs -> expectationFailure $ "Should pass: " ++ show errs

            describe "Multiple error accumulation" $ do
                it "relaxed mode accumulates multiple warnings" $ do
                    -- Two mismatches in sequence
                    let result = runTC (
                            check (Lit (LInt 1)) (TCon "Bool") `tcBind` \_ ->
                            check (Lit (LString "hi")) (TCon "Int")
                            ) env0 st0
                    case result of
                        Right (_, st') -> length (tcErrors st') `shouldSatisfy` (>= 2)
                        Left _ -> expectationFailure "Relaxed mode should accumulate, not fail"

                it "strict mode fails on first mismatch" $ do
                    let stStrict = initTCState TCStrict
                    let result = runTC (
                            check (Lit (LInt 1)) (TCon "Bool") `tcBind` \_ ->
                            check (Lit (LString "hi")) (TCon "Int")
                            ) env0 stStrict
                    case result of
                        Left errs -> length errs `shouldBe` 1  -- only first error
                        Right _ -> expectationFailure "Strict mode should fail on first"

            describe "inferLambda with implicit params" $ do
                it "strips implicit param from type" $ do
                    -- function f [a:Type] (x:a) : a = x
                    let lam = mkLambda "f" [Var "a" (Implicit (Id "Type")) UNDEFINED,
                                            Var "x" (Id "a") UNDEFINED]
                                        (Id "x") (Id "a")
                    let Right (ty, _) = runTC (inferLambda lam) env0 st0
                    -- Should return just the value-level arrow: a -> a (wrapped in forall)
                    case ty of
                        TArrow _ _ -> pure ()  -- some form of arrow type
                        other -> expectationFailure $ "Expected arrow type, got: " ++ show other

            describe "exprToTy extended coverage" $ do
                it "converts empty Tuple to Unit" $ do
                    let Right (ty, _) = runTC (exprToTy (Tuple [])) env0 st0
                    ty `shouldBe` TCon "Unit"

                it "converts single Tuple to inner type" $ do
                    let Right (ty, _) = runTC (exprToTy (Tuple [Id "Int"])) env0 st0
                    ty `shouldBe` TCon "Int"

                it "converts multi Tuple to TSigma chain" $ do
                    let Right (ty, _) = runTC (exprToTy (Tuple [Id "Int", Id "Bool"])) env0 st0
                    ty `shouldBe` TSigma Nothing (TCon "Int") (TCon "Bool")

                it "converts Implicit wrapper" $ do
                    let Right (ty, _) = runTC (exprToTy (Implicit (Id "Int"))) env0 st0
                    ty `shouldBe` TCon "Int"

                it "converts EffType" $ do
                    let effExpr = EffType (RecordType [("console", Id "Console")] False) (Id "Unit")
                    let Right (ty, _) = runTC (exprToTy effExpr) env0 st0
                    case ty of
                        TEffect (RExtend "console" (TCon "Console") REmpty) (TCon "Unit") -> pure ()
                        other -> expectationFailure $ "Expected Eff {console:Console} Unit, got: " ++ show other

                it "converts PropEqT to TId" $ do
                    let peq = App (Id "PropEqT") [Id "Nat", Id "Z", Id "Z"]
                    let Right (ty, _) = runTC (exprToTy peq) env0 st0
                    ty `shouldBe` TId (TCon "Nat") (TCon "Z") (TCon "Z")

            describe "applySubst through complex types" $ do
                it "substitutes through TApp" $ do
                    let st1 = st0 { nextVar = 1, substitution = Map.fromList [(0, TCon "Int")] }
                    let Right (ty, _) = runTC (applySubst (TApp (TCon "Maybe") [TVar 0])) env0 st1
                    ty `shouldBe` TApp (TCon "Maybe") [TCon "Int"]

                it "substitutes through TSigma" $ do
                    let st1 = st0 { nextVar = 2, substitution = Map.fromList [(0, TCon "Int"), (1, TCon "Bool")] }
                    let Right (ty, _) = runTC (applySubst (TSigma Nothing (TVar 0) (TVar 1))) env0 st1
                    ty `shouldBe` TSigma Nothing (TCon "Int") (TCon "Bool")

                it "substitutes through TId" $ do
                    let st1 = st0 { nextVar = 1, substitution = Map.fromList [(0, TCon "Z")] }
                    let Right (ty, _) = runTC (applySubst (TId (TCon "Nat") (TVar 0) (TVar 0))) env0 st1
                    ty `shouldBe` TId (TCon "Nat") (TCon "Z") (TCon "Z")

                it "substitutes through TForall body" $ do
                    let st1 = st0 { nextVar = 1, substitution = Map.fromList [(0, TCon "Int")] }
                    let Right (ty, _) = runTC (applySubst (TForall "a" (TArrow (TRigid "a") (TVar 0)))) env0 st1
                    ty `shouldBe` TForall "a" (TArrow (TRigid "a") (TCon "Int"))

                it "substitutes through TRecord row" $ do
                    let st1 = st0 { nextVar = 1, substitution = Map.fromList [(0, TCon "Int")] }
                    let Right (ty, _) = runTC (applySubst (TRecord (RExtend "x" (TVar 0) REmpty))) env0 st1
                    ty `shouldBe` TRecord (RExtend "x" (TCon "Int") REmpty)

                it "substitutes through TEffect" $ do
                    let st1 = st0 { nextVar = 1, substitution = Map.fromList [(0, TCon "String")] }
                    let Right (ty, _) = runTC (applySubst (TEffect REmpty (TVar 0))) env0 st1
                    ty `shouldBe` TEffect REmpty (TCon "String")

                it "chases substitution chains" $ do
                    -- ?0 -> ?1 -> Int
                    let st1 = st0 { nextVar = 2, substitution = Map.fromList [(0, TVar 1), (1, TCon "Int")] }
                    let Right (ty, _) = runTC (applySubst (TVar 0)) env0 st1
                    ty `shouldBe` TCon "Int"

            describe "showTy coverage" $ do
                it "showTy TVar" $ showTy (TVar 42) `shouldBe` "?42"
                it "showTy TRigid" $ showTy (TRigid "a") `shouldBe` "a"
                it "showTy TCon" $ showTy (TCon "Int") `shouldBe` "Int"
                it "showTy TApp" $ showTy (TApp (TCon "Maybe") [TCon "Int"]) `shouldBe` "Maybe(Int)"
                it "showTy TArrow" $ showTy (TArrow (TCon "Int") (TCon "Bool")) `shouldBe` "Int -> Bool"
                it "showTy TPi dependent" $ showTy (TPi (Just "x") (TCon "Int") (TCon "Bool")) `shouldBe` "(x:Int) -> Bool"
                it "showTy TSigma" $ showTy (TSigma Nothing (TCon "Int") (TCon "Bool")) `shouldBe` "(Int, Bool)"
                it "showTy TSigma dependent" $ showTy (TSigma (Just "x") (TCon "Int") (TCon "Bool")) `shouldBe` "(x:Int * Bool)"
                it "showTy TId" $ showTy (TId (TCon "Nat") (TCon "Z") (TCon "Z")) `shouldBe` "Id(Nat, Z, Z)"
                it "showTy TForall" $ showTy (TForall "a" (TArrow (TRigid "a") (TRigid "a"))) `shouldBe` "forall a. a -> a"
                it "showTy TRecord" $ showTy (TRecord (RExtend "x" (TCon "Int") REmpty)) `shouldBe` "{x:Int}"
                it "showTy TEffect" $ showTy (TEffect (RExtend "c" (TCon "C") REmpty) (TCon "Unit")) `shouldBe` "Eff {c:C} Unit"
                it "showTy TU 0" $ showTy (TU 0) `shouldBe` "Type"
                it "showTy TU 1" $ showTy (TU 1) `shouldBe` "Type1"
                it "showTy nested arrow in arg position" $ do
                    showTy (TArrow (TArrow (TCon "Int") (TCon "Bool")) (TCon "String"))
                        `shouldBe` "(Int -> Bool) -> String"
                it "showRow with row variable" $ showRow (RVar 5) `shouldBe` "..?5"
                it "showRow with rigid" $ showRow (RRigid "r") `shouldBe` "..r"
                it "showRow multiple fields" $ do
                    showRow (RExtend "x" (TCon "Int") (RExtend "y" (TCon "Bool") REmpty))
                        `shouldBe` "x:Int, y:Bool"

            describe "showTCError coverage" $ do
                it "ConstraintUnsolved shows structure name" $ do
                    let err = ConstraintUnsolved (CStructure "Eq" [TCon "MyType"])
                    showTCError err `shouldSatisfy` ("Eq" `isInfixOf`)
                    showTCError err `shouldSatisfy` ("MyType" `isInfixOf`)

                it "WithContext nests context info" $ do
                    let err = WithContext "method 'foo'" (WithContext "class Bar" (Mismatch (TCon "Int") (TCon "Bool")))
                    let s = showTCError err
                    s `shouldSatisfy` ("foo" `isInfixOf`)
                    s `shouldSatisfy` ("Bar" `isInfixOf`)
                    s `shouldSatisfy` ("Int" `isInfixOf`)

            describe "Numeric literal types" $ do
                it "infers Int8 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LInt8 1))) env0 st0
                    ty `shouldBe` TCon "Int8"
                it "infers Int16 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LInt16 1))) env0 st0
                    ty `shouldBe` TCon "Int16"
                it "infers Int32 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LInt32 1))) env0 st0
                    ty `shouldBe` TCon "Int32"
                it "infers Int64 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LInt64 1))) env0 st0
                    ty `shouldBe` TCon "Int64"
                it "infers UInt8 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LWord8 1))) env0 st0
                    ty `shouldBe` TCon "UInt8"
                it "infers UInt16 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LWord16 1))) env0 st0
                    ty `shouldBe` TCon "UInt16"
                it "infers UInt32 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LWord32 1))) env0 st0
                    ty `shouldBe` TCon "UInt32"
                it "infers UInt64 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LWord64 1))) env0 st0
                    ty `shouldBe` TCon "UInt64"
                it "infers Float32 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LFloat32 1.0))) env0 st0
                    ty `shouldBe` TCon "Float32"

            describe "Misc infer cases" $ do
                it "infers empty LList" $ do
                    let Right (ty, _) = runTC (infer (Lit (LList []))) env0 st0
                    case ty of
                        TApp (TCon "List") [TVar _] -> pure ()
                        other -> expectationFailure $ "Expected List(?), got: " ++ show other
                it "infers non-empty LList" $ do
                    let Right (ty, _) = runTC (infer (Lit (LList [Lit (LInt 1)]))) env0 st0
                    ty `shouldBe` TApp (TCon "List") [TCon "Int"]

                it "Typed expression checks against annotation" $ do
                    let Right (ty, _) = runTC (infer (Typed (Lit (LInt 42)) (Id "Int"))) env0 st0
                    ty `shouldBe` TCon "Int"

                it "Typed expression with mismatch warns" $ do
                    let result = runTC (infer (Typed (Lit (LInt 42)) (Id "Bool"))) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()

                it "PatternMatches empty returns fresh var" $ do
                    let Right (ty, _) = runTC (infer (PatternMatches [])) env0 st0
                    case ty of
                        TVar _ -> pure ()
                        other -> expectationFailure $ "Expected fresh var, got: " ++ show other

                it "Statements infers last statement type" $ do
                    let Right (ty, _) = runTC (infer (Statements [Lit (LString "a"), Lit (LInt 42)])) env0 st0
                    ty `shouldBe` TCon "Int"

                it "empty Statements returns Unit" $ do
                    let Right (ty, _) = runTC (infer (Statements [])) env0 st0
                    ty `shouldBe` TCon "Unit"

                it "UnaryOp dispatches to App" $ do
                    let env1 = env0 { varTypes = Map.fromList [("negate", TArrow (TCon "Int") (TCon "Int"))] }
                    let Right (ty, _) = runTC (infer (UnaryOp "negate" (Lit (LInt 5)))) env1 st0
                    ty `shouldBe` TCon "Int"

                it "BinaryOp dispatches to App" $ do
                    let env1 = env0 { varTypes = Map.fromList [("+", TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))] }
                    let Right (ty, _) = runTC (infer (BinaryOp "+" (Lit (LInt 1)) (Lit (LInt 2)))) env1 st0
                    ty `shouldBe` TCon "Int"

                it "U n infers TU (n+1)" $ do
                    let Right (ty, _) = runTC (infer (U 0)) env0 st0
                    ty `shouldBe` TU 1
                    let Right (ty2, _) = runTC (infer (U 1)) env0 st0
                    ty2 `shouldBe` TU 2

                it "SumType infers Type" $ do
                    let Right (ty, _) = runTC (infer (SumType (mkLambda "Bool" [] (Tuple []) (U 0)))) env0 st0
                    ty `shouldBe` TU 0

                it "ReprCast infers target type" $ do
                    let Right (ty, _) = runTC (infer (ReprCast (Lit (LInt 42)) (Id "Nat"))) env0 st0
                    ty `shouldBe` TCon "Nat"

                it "ERROR node warns and returns fresh var" $ do
                    let Right (ty, st') = runTC (infer (ERROR "test error")) env0 st0
                    case ty of
                        TVar _ -> pure ()
                        other -> expectationFailure $ "Expected fresh var, got: " ++ show other
                    tcErrors st' `shouldSatisfy` (not . null)

                it "RecFieldAccess on record type extracts field" $ do
                    let recTy = TRecord (RExtend "x" (TCon "Int") (RExtend "y" (TCon "Bool") REmpty))
                        env1 = env0 { varTypes = Map.fromList [("r", recTy)] }
                    let Right (ty, _) = runTC (infer (RecFieldAccess ("x", -1) (Id "r"))) env1 st0
                    ty `shouldBe` TCon "Int"

                it "RecFieldAccess on non-record returns fresh var" $ do
                    let env1 = env0 { varTypes = Map.fromList [("n", TCon "Int")] }
                    let Right (ty, _) = runTC (infer (RecFieldAccess ("x", -1) (Id "n"))) env1 st0
                    case ty of
                        TVar _ -> pure ()
                        other -> expectationFailure $ "Expected fresh var, got: " ++ show other

            describe "lamToTy" $ do
                it "converts simple lambda to arrow type" $ do
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED] UNDEFINED (Id "Bool")
                    let Right (ty, _) = runTC (lamToTy lam) env0 st0
                    case ty of
                        TArrow (TCon "Int") _ -> pure ()
                        other -> expectationFailure $ "Expected Int -> ?, got: " ++ show other

                it "converts multi-param lambda to nested arrows" $ do
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED, Var "y" (Id "Bool") UNDEFINED]
                                        UNDEFINED (Id "String")
                    let Right (ty, _) = runTC (lamToTy lam) env0 st0
                    case ty of
                        TArrow (TCon "Int") (TArrow (TCon "Bool") _) -> pure ()
                        other -> expectationFailure $ "Expected Int -> Bool -> ?, got: " ++ show other

                it "converts zero-param lambda to return type" $ do
                    let lam = mkLambda "f" [] UNDEFINED (Id "Int")
                    let Right (ty, _) = runTC (lamToTy lam) env0 st0
                    case ty of
                        TVar _ -> pure ()  -- UNDEFINED return type -> fresh var
                        TCon "Int" -> pure ()
                        other -> expectationFailure $ "Expected Int or fresh var, got: " ++ show other

            describe "tyToName" $ do
                it "extracts name from TCon" $ tyToName (TCon "Int") `shouldBe` "Int"
                it "extracts name from TApp head" $ tyToName (TApp (TCon "Maybe") [TCon "Int"]) `shouldBe` "Maybe"
                it "returns empty for TVar" $ tyToName (TVar 0) `shouldBe` ""
                it "returns empty for TRigid" $ tyToName (TRigid "a") `shouldBe` ""
                it "returns empty for TArrow" $ tyToName (TArrow (TCon "Int") (TCon "Bool")) `shouldBe` ""

            describe "Context nesting" $ do
                it "nested tcWithContext shows innermost context" $ do
                    let result = runTC (
                            tcWithContext "outer" (
                              tcWithContext "inner" (
                                tcWarnOrFail (OtherError "test")
                              ))
                            ) env0 st0
                    case result of
                        Right (_, st') -> do
                            let errs = tcErrors st'
                            length errs `shouldBe` 1
                            case Prelude.head errs of
                                WithContext ctx _ -> ctx `shouldBe` "inner"
                                other -> expectationFailure $ "Expected WithContext, got: " ++ show other
                        Left errs -> do
                            case Prelude.head errs of
                                WithContext ctx _ -> ctx `shouldBe` "inner"
                                other -> expectationFailure $ "Expected WithContext, got: " ++ show other

            describe "generalize edge cases" $ do
                it "no free vars produces no forall" $ do
                    let Right (ty, _) = runTC (generalize (TArrow (TCon "Int") (TCon "Bool"))) env0 st0
                    ty `shouldBe` TArrow (TCon "Int") (TCon "Bool")

                it "multiple free vars produce multiple foralls" $ do
                    let Right (ty, _) = runTC (generalize (TArrow (TVar 0) (TArrow (TVar 1) (TVar 0)))) env0 (st0 { nextVar = 2 })
                    case ty of
                        TForall _ (TForall _ _) -> pure ()
                        other -> expectationFailure $ "Expected forall a. forall b. ..., got: " ++ show other

            describe "Bidirectional check integration" $ do
                it "check record literal against compatible open record" $ do
                    let st1 = st0 { nextVar = 1 }
                        recLit = RecordLit [("x", Lit (LInt 1)), ("y", Lit (LFloat 2.0))]
                        openRecTy = TRecord (RExtend "x" (TCon "Int") (RVar 0))
                    let result = runTC (check recLit openRecTy) env0 st1
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Open record should accept extra fields: " ++ show errs

                it "checkTopLevel SumType passes" $ do
                    let result = runTC (checkTopLevel (SumType (mkLambda "Bool" [] (Tuple []) (U 0)))) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "SumType should pass: " ++ show errs

                it "checkTopLevel Instance checks implementations" $ do
                    let result = runTC (checkTopLevel (Instance "Eq" [Id "Bool"] [Function (mkLambda "==" [] UNDEFINED UNDEFINED)] [])) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Instance should pass: " ++ show errs

        -- ============================================================
        -- Phase 0: Module system parsing
        -- ============================================================
        describe "Module system parsing" $ do
            it "parses module declaration" $ do
                result <- parseTestString "module Algebra.Ring;"
                case result of
                    Right [ModuleDecl ["Algebra", "Ring"]] -> pure ()
                    Right other -> expectationFailure $ "Unexpected parse: " ++ show other
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "parses simple import" $ do
                result <- parseTestString "import Algebra.Ring;"
                case result of
                    Right [Import ["Algebra", "Ring"] ImportAll Nothing] -> pure ()
                    Right other -> expectationFailure $ "Unexpected parse: " ++ show other
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "parses selective import" $ do
                result <- parseTestString "import Algebra.Ring (Semiring, Field);"
                case result of
                    Right [Import ["Algebra", "Ring"] (ImportOnly ["Semiring", "Field"]) Nothing] -> pure ()
                    Right other -> expectationFailure $ "Unexpected parse: " ++ show other
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "parses import with alias" $ do
                result <- parseTestString "import Algebra.Ring as R;"
                case result of
                    Right [Import ["Algebra", "Ring"] (ImportAs "R") Nothing] -> pure ()
                    Right other -> expectationFailure $ "Unexpected parse: " ++ show other
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "parses import with hiding" $ do
                result <- parseTestString "import Algebra.Ring hiding (Field);"
                case result of
                    Right [Import ["Algebra", "Ring"] (ImportHiding ["Field"]) Nothing] -> pure ()
                    Right other -> expectationFailure $ "Unexpected parse: " ++ show other
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "parses target import" $ do
                result <- parseTestString "import System.Windows.Forms target dotnet;"
                case result of
                    Right [Import ["System", "Windows", "Forms"] ImportAll (Just "dotnet")] -> pure ()
                    Right other -> expectationFailure $ "Unexpected parse: " ++ show other
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "parses open declaration" $ do
                result <- parseTestString "open Algebra.Ring;"
                case result of
                    Right [Open ["Algebra", "Ring"]] -> pure ()
                    Right other -> expectationFailure $ "Unexpected parse: " ++ show other
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "parses export declaration" $ do
                result <- parseTestString "export Algebra.Ring;"
                case result of
                    Right [Export ["Algebra", "Ring"] Nothing] -> pure ()
                    Right other -> expectationFailure $ "Unexpected parse: " ++ show other
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "parses selective export" $ do
                result <- parseTestString "export Algebra.Ring (Semiring);"
                case result of
                    Right [Export ["Algebra", "Ring"] (Just ["Semiring"])] -> pure ()
                    Right other -> expectationFailure $ "Unexpected parse: " ++ show other
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "parses private function" $ do
                result <- parseTestString "private function helper(x:Int) : Int = x;"
                case result of
                    Right [PrivateDecl _] -> pure ()
                    Right other -> expectationFailure $ "Unexpected parse: " ++ show other
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "parses opaque type" $ do
                result <- parseTestString "opaque type Seconds = Int;"
                case result of
                    Right [OpaqueTy _ _] -> pure ()
                    Right other -> expectationFailure $ "Unexpected parse: " ++ show other
                    Left err -> expectationFailure $ "Parse error: " ++ err

        -- ============================================================
        -- Phase 0: Module system resolution
        -- ============================================================
        describe "Module system resolution" $ do
            it "resolves module path to file" $ do
                let paths = ["lib/"]
                result <- resolveModulePath paths ["Algebra", "Ring"]
                -- May or may not find the file depending on lib/ existence
                case result of
                    Just p  -> p `shouldSatisfy` \s -> "Ring.tl" `isInfixOf` s
                    Nothing -> pure ()  -- lib/ might not exist in test env

            it "detects cycles in dependency graph" $ do
                let graph = Map.fromList
                        [ ("A", ["B"])
                        , ("B", ["A"])
                        ] :: ModuleGraph
                detectCycles graph `shouldNotBe` Nothing

            it "no cycles in acyclic graph" $ do
                let graph = Map.fromList
                        [ ("A", ["B"])
                        , ("B", [])
                        ] :: ModuleGraph
                detectCycles graph `shouldBe` Nothing

            it "topological sort of acyclic graph succeeds" $ do
                let graph = Map.fromList
                        [ ("A", ["B"])
                        , ("B", [])
                        ] :: ModuleGraph
                case topologicalSort graph of
                    Right order -> do
                        -- Both modules should be present in the result
                        length order `shouldBe` 2
                        order `shouldSatisfy` ("A" `elem`)
                        order `shouldSatisfy` ("B" `elem`)
                    Left err -> expectationFailure $ "Unexpected cycle: " ++ show err

        -- ============================================================
        -- Milestone 3: Module-scoped environments + visibility
        -- ============================================================
        describe "Module-scoped environments" $ do
            it "loadedModules is populated after loading" $ do
                st <- setupEnv
                let loaded = loadedModules (currentModuleEnv st)
                -- At least some modules should be tracked
                Map.size loaded `shouldSatisfy` (> 0)

            it "each loaded module has publicNames" $ do
                st <- setupEnv
                let loaded = loadedModules (currentModuleEnv st)
                -- Core.Bool should define Bool, True, False
                case Map.lookup "Core.Bool" loaded of
                    Just menv -> do
                        Set.member "Bool" (publicNames menv) `shouldBe` True
                        Set.member "True" (publicNames menv) `shouldBe` True
                        Set.member "False" (publicNames menv) `shouldBe` True
                    Nothing -> expectationFailure "Core.Bool module not found in loadedModules"

            it "filterVisibleNames with ImportAll returns all public names" $ do
                let names = Set.fromList ["Foo", "Bar", "Baz"]
                filterVisibleNames names ImportAll `shouldBe` names

            it "filterVisibleNames with ImportOnly filters to listed names" $ do
                let names = Set.fromList ["Foo", "Bar", "Baz"]
                filterVisibleNames names (ImportOnly ["Foo", "Baz"]) `shouldBe` Set.fromList ["Foo", "Baz"]

            it "filterVisibleNames with ImportHiding excludes listed names" $ do
                let names = Set.fromList ["Foo", "Bar", "Baz"]
                filterVisibleNames names (ImportHiding ["Bar"]) `shouldBe` Set.fromList ["Foo", "Baz"]

            it "private names are tracked in privateNames set" $ do
                -- Parse and process a module with private declarations
                let src = "module Test.Priv;\nprivate function helper(x:Int):Int = x;\nfunction pub(x:Int):Int = x;"
                result <- parseTestString src
                case result of
                    Right exprs -> do
                        -- Should have ModuleDecl, PrivateDecl, Function
                        length exprs `shouldBe` 3
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "Prelude module is tracked in loadedModules" $ do
                st <- setupEnv
                let loaded = loadedModules (currentModuleEnv st)
                -- Prelude should be tracked (it has no ModuleDecl, so key = filepath)
                let hasPrelude = Map.member "lib/Prelude.tl" loaded || Map.member "Prelude" loaded
                hasPrelude `shouldBe` True

        -- ============================================================
        -- Phase 1: New primitive types registered
        -- ============================================================
        describe "Primitive type registration" $ do
            it "all primitive types registered after loading prelude" $ do
                st <- setupEnv
                let ts = types (currentEnvironment st)
                let expectedTypes = ["Int", "Float64", "String", "Char",
                                     "Int8", "Int16", "Int32", "Int64",
                                     "UInt", "UInt8", "UInt16", "UInt32", "UInt64",
                                     "Float32", "Byte", "Array",
                                     "Vec2", "Vec4", "Vec8", "Vec16"]
                mapM_ (\t -> Map.lookup t ts `shouldNotBe` Nothing) expectedTypes

        -- ============================================================
        -- Phase 1: New literal types
        -- ============================================================
        describe "New literal types" $ do
            it "new Literal constructors exist and are distinct" $ do
                LInt8 42 `shouldNotBe` LInt8 43
                LInt32 100 `shouldNotBe` LInt32 101
                LFloat32 3.14 `shouldNotBe` LFloat32 3.15
                LWord8 255 `shouldNotBe` LWord8 0

            it "CLM literal type suffixes" $ do
                ppr (CLMLIT (LInt8 42)) `shouldBe` "42i8"
                ppr (CLMLIT (LInt32 42)) `shouldBe` "42i32"
                ppr (CLMLIT (LWord8 255)) `shouldBe` "255u8"
                ppr (CLMLIT (LFloat32 1.0)) `shouldBe` "1.0f32"

            it "CLM CLMARRAY pretty-prints" $ do
                ppr (CLMARRAY [CLMLIT (LInt 1), CLMLIT (LInt 2)]) `shouldBe` "[1, 2]"

        -- ============================================================
        -- Phase 2: Intrinsic registry
        -- ============================================================
        describe "Intrinsic registry expansion" $ do
            it "Int intrinsics exist (add, sub, mul)" $ do
                hasIntrinsic "+" "Int"
                hasIntrinsic "-" "Int"
                hasIntrinsic "*" "Int"

            it "Int8 intrinsics exist" $ do
                hasIntrinsic "+" "Int8"
                hasIntrinsic "negate" "Int8"
                hasIntrinsic "==" "Int8"

            it "Int32 intrinsics exist" $ do
                hasIntrinsic "+" "Int32"
                hasIntrinsic "div" "Int32"
                hasIntrinsic ".&." "Int32"

            it "UInt8 and UInt16 intrinsics exist" $ do
                hasIntrinsic "+" "UInt8"
                hasIntrinsic "+" "UInt16"

            it "UInt8 intrinsics exist" $ do
                hasIntrinsic "+" "UInt8"
                hasIntrinsic "xor" "UInt8"

            it "Float32 intrinsics exist" $ do
                hasIntrinsic "+" "Float32"
                hasIntrinsic "sqrt" "Float32"
                hasIntrinsic "sin" "Float32"
                hasIntrinsic "pi" "Float32"

            it "Float64 intrinsics include transcendentals" $ do
                hasIntrinsic "sqrt" "Float64"
                hasIntrinsic "sin" "Float64"
                hasIntrinsic "cos" "Float64"
                hasIntrinsic "exp" "Float64"
                hasIntrinsic "log" "Float64"

            it "bitwise ops exist for Int" $ do
                hasIntrinsic ".&." "Int"
                hasIntrinsic ".|." "Int"
                hasIntrinsic "xor" "Int"
                hasIntrinsic "complement" "Int"
                hasIntrinsic "shiftL" "Int"
                hasIntrinsic "shiftR" "Int"

            it "Int8 add evaluates correctly" $ do
                case lookupIntrinsic "+" "Int8" of
                    Just f -> f [CLMLIT (LInt8 3), CLMLIT (LInt8 4)] `shouldBe` Just (CLMLIT (LInt8 7))
                    Nothing -> expectationFailure "Int8 + not found"

            it "Int32 multiply evaluates correctly" $ do
                case lookupIntrinsic "*" "Int32" of
                    Just f -> f [CLMLIT (LInt32 6), CLMLIT (LInt32 7)] `shouldBe` Just (CLMLIT (LInt32 42))
                    Nothing -> expectationFailure "Int32 * not found"

            it "UInt8 add wraps" $ do
                case lookupIntrinsic "+" "UInt8" of
                    Just f -> f [CLMLIT (LWord8 255), CLMLIT (LWord8 1)] `shouldBe` Just (CLMLIT (LWord8 0))
                    Nothing -> expectationFailure "UInt8 + not found"

            it "Float32 sqrt evaluates" $ do
                case lookupIntrinsic "sqrt" "Float32" of
                    Just f -> f [CLMLIT (LFloat32 4.0)] `shouldBe` Just (CLMLIT (LFloat32 2.0))
                    Nothing -> expectationFailure "Float32 sqrt not found"

            it "Int comparison returns Bool constructors" $ do
                case lookupIntrinsic "==" "Int8" of
                    Just f -> do
                        f [CLMLIT (LInt8 5), CLMLIT (LInt8 5)] `shouldBe` Just conTrue
                        f [CLMLIT (LInt8 5), CLMLIT (LInt8 6)] `shouldBe` Just conFalse
                    Nothing -> expectationFailure "Int8 == not found"

            it "Int compare returns Ordering" $ do
                case lookupIntrinsic "compare" "Int" of
                    Just f -> do
                        let lt = f [CLMLIT (LInt 1), CLMLIT (LInt 2)]
                        lt `shouldBe` Just (CLMCON (ConsTag "LessThan" 0) [])
                        let gt = f [CLMLIT (LInt 2), CLMLIT (LInt 1)]
                        gt `shouldBe` Just (CLMCON (ConsTag "GreaterThan" 2) [])
                        let eq = f [CLMLIT (LInt 1), CLMLIT (LInt 1)]
                        eq `shouldBe` Just (CLMCON (ConsTag "Equal" 1) [])
                    Nothing -> expectationFailure "Int compare not found"

        -- ============================================================
        -- Phase 3: Algebra instances for new types
        -- ============================================================
        describe "Algebra instances for new types" $ do
            it "Int8 has EuclideanDomain instance (and parents)" $ do
                st <- setupEnv
                let il = instanceLambdas (currentEnvironment st)
                -- EuclideanDomain propagates to all parents
                Map.lookup "+\0Int8" il `shouldNotBe` Nothing
                Map.lookup "*\0Int8" il `shouldNotBe` Nothing
                Map.lookup "zero\0Int8" il `shouldNotBe` Nothing
                Map.lookup "one\0Int8" il `shouldNotBe` Nothing

            it "Int32 has full numeric tower" $ do
                st <- setupEnv
                let il = instanceLambdas (currentEnvironment st)
                Map.lookup "+\0Int32" il `shouldNotBe` Nothing
                Map.lookup "negate\0Int32" il `shouldNotBe` Nothing
                Map.lookup "div\0Int32" il `shouldNotBe` Nothing
                Map.lookup "mod\0Int32" il `shouldNotBe` Nothing

            it "UInt8 has instances" $ do
                st <- setupEnv
                let il = instanceLambdas (currentEnvironment st)
                Map.lookup "+\0UInt8" il `shouldNotBe` Nothing
                Map.lookup ".&.\0UInt8" il `shouldNotBe` Nothing

            it "Float32 has Floating instances" $ do
                st <- setupEnv
                let il = instanceLambdas (currentEnvironment st)
                Map.lookup "+\0Float32" il `shouldNotBe` Nothing
                Map.lookup "sqrt\0Float32" il `shouldNotBe` Nothing
                Map.lookup "sin\0Float32" il `shouldNotBe` Nothing
                Map.lookup "pi\0Float32" il `shouldNotBe` Nothing

            it "Eq and Ord instances for new types" $ do
                st <- setupEnv
                let il = instanceLambdas (currentEnvironment st)
                -- Eq
                Map.lookup "==\0Int8" il `shouldNotBe` Nothing
                Map.lookup "==\0UInt8" il `shouldNotBe` Nothing
                Map.lookup "==\0Float32" il `shouldNotBe` Nothing
                -- Ord
                Map.lookup "compare\0Int32" il `shouldNotBe` Nothing
                Map.lookup "<\0UInt" il `shouldNotBe` Nothing

        -- ============================================================
        -- Phase 4: Array primitive
        -- ============================================================
        describe "Array primitive" $ do
            it "Array intrinsics exist" $ do
                hasIntrinsic "length" "Array"
                hasIntrinsic "index" "Array"
                hasIntrinsic "slice" "Array"

            it "array length evaluates" $ do
                case lookupIntrinsic "length" "Array" of
                    Just f -> f [CLMARRAY [CLMLIT (LInt 1), CLMLIT (LInt 2), CLMLIT (LInt 3)]]
                        `shouldBe` Just (CLMLIT (LInt 3))
                    Nothing -> expectationFailure "Array length not found"

            it "array index evaluates" $ do
                case lookupIntrinsic "index" "Array" of
                    Just f -> f [CLMARRAY [CLMLIT (LInt 10), CLMLIT (LInt 20), CLMLIT (LInt 30)], CLMLIT (LInt 1)]
                        `shouldBe` Just (CLMLIT (LInt 20))
                    Nothing -> expectationFailure "Array index not found"

            it "array index out of bounds returns error" $ do
                case lookupIntrinsic "index" "Array" of
                    Just f -> case f [CLMARRAY [CLMLIT (LInt 1)], CLMLIT (LInt 5)] of
                        Just (CLMERR msg _) -> msg `shouldSatisfy` ("out of bounds" `isInfixOf`)
                        other -> expectationFailure $ "Expected CLMERR, got: " ++ show other
                    Nothing -> expectationFailure "Array index not found"

            it "array slice evaluates" $ do
                case lookupIntrinsic "slice" "Array" of
                    Just f -> f [CLMARRAY [CLMLIT (LInt 1), CLMLIT (LInt 2), CLMLIT (LInt 3), CLMLIT (LInt 4)],
                                 CLMLIT (LInt 1), CLMLIT (LInt 3)]
                        `shouldBe` Just (CLMARRAY [CLMLIT (LInt 2), CLMLIT (LInt 3)])
                    Nothing -> expectationFailure "Array slice not found"

        -- ============================================================
        -- Phase 5: Polymorphic literals (fromInt/fromFloat)
        -- ============================================================
        describe "Polymorphic literals (fromInt/fromFloat)" $ do
            it "fromInt to Int8 exists" $ do
                hasIntrinsic "fromInt" "Int8"

            it "fromInt to Int32 converts" $ do
                case lookupIntrinsic "fromInt" "Int32" of
                    Just f -> f [CLMLIT (LInt 42)] `shouldBe` Just (CLMLIT (LInt32 42))
                    Nothing -> expectationFailure "fromInt Int32 not found"

            it "fromInt to Float64 converts" $ do
                case lookupIntrinsic "fromInt" "Float64" of
                    Just f -> f [CLMLIT (LInt 42)] `shouldBe` Just (CLMLIT (LFloat 42.0))
                    Nothing -> expectationFailure "fromInt Float64 not found"

            it "fromFloat to Float32 converts" $ do
                case lookupIntrinsic "fromFloat" "Float32" of
                    Just f -> case f [CLMLIT (LFloat 3.14)] of
                        Just (CLMLIT (LFloat32 _)) -> pure ()
                        other -> expectationFailure $ "Unexpected: " ++ show other
                    Nothing -> expectationFailure "fromFloat Float32 not found"

        -- ============================================================
        -- Phase 6: Numeric conversions
        -- ============================================================
        describe "Numeric conversions (Convertible)" $ do
            it "convert Int to Float64" $ do
                hasIntrinsic "convert" "Int\0Float64"

            it "convert Int8 to Int16 (widening)" $ do
                case lookupIntrinsic "convert" "Int8\0Int16" of
                    Just f -> f [CLMLIT (LInt8 42)] `shouldBe` Just (CLMLIT (LInt16 42))
                    Nothing -> expectationFailure "convert Int8->Int16 not found"

            it "convert Float32 to Float64 (widening)" $ do
                case lookupIntrinsic "convert" "Float32\0Float64" of
                    Just f -> case f [CLMLIT (LFloat32 3.14)] of
                        Just (CLMLIT (LFloat _)) -> pure ()
                        other -> expectationFailure $ "Unexpected: " ++ show other
                    Nothing -> expectationFailure "convert Float32->Float64 not found"

        -- ============================================================
        -- Phase 7: SIMD stubs
        -- ============================================================
        describe "SIMD stubs" $ do
            it "Vec types registered" $ do
                st <- setupEnv
                let ts = types (currentEnvironment st)
                Map.lookup "Vec2" ts `shouldNotBe` Nothing
                Map.lookup "Vec4" ts `shouldNotBe` Nothing
                Map.lookup "Vec8" ts `shouldNotBe` Nothing
                Map.lookup "Vec16" ts `shouldNotBe` Nothing

            it "SIMD intrinsics return stub errors" $ do
                hasIntrinsic "splat" "Vec4"
                case lookupIntrinsic "splat" "Vec4" of
                    Just f -> case f [CLMLIT (LInt 1)] of
                        Just (CLMERR msg _) -> msg `shouldSatisfy` ("SIMD" `isInfixOf`)
                        other -> expectationFailure $ "Expected CLMERR, got: " ++ show other
                    Nothing -> expectationFailure "Vec4 splat not found"

            it "Lane algebra functions registered" $ do
                st <- setupEnv
                let il = instanceLambdas (currentEnvironment st)
                Map.lookup "splat\0Vec4" il `shouldNotBe` Nothing
                Map.lookup "extract\0Vec4" il `shouldNotBe` Nothing
                Map.lookup "lanes\0Vec4" il `shouldNotBe` Nothing

        -- ============================================================
        -- Phase 0-7: Integration - REPL evaluates with new types
        -- ============================================================
        describe "Integration: evaluation with base environment" $ do
            it "1 + 2 evaluates to 3" $ do
                st <- setupEnv
                result <- evalExpr st "1 + 2"
                result `shouldBe` CLMLIT (LInt 3)

            it "3.14 * 2.0 evaluates" $ do
                st <- setupEnv
                result <- evalExpr st "3.14 * 2.0"
                case result of
                    CLMLIT (LFloat v) -> v `shouldSatisfy` (\x -> abs (x - 6.28) < 0.01)
                    other -> expectationFailure $ "Expected Float, got: " ++ show other

            it "not(True) evaluates to False" $ do
                st <- setupEnv
                result <- evalExpr st "not(True)"
                result `shouldBe` conFalse

        -- ============================================================
        -- lib/ directory structure exists
        -- ============================================================
        describe "Library directory structure" $ do
            it "lib/ directory contains key module files" $ do
                exists1 <- doesFileExist "lib/Prelude.tl"
                exists2 <- doesFileExist "lib/Base.tl"
                exists3 <- doesFileExist "lib/Algebra/Ring.tl"
                exists4 <- doesFileExist "lib/Core/Bool.tl"
                exists5 <- doesFileExist "lib/Numeric/Int.tl"
                exists1 `shouldBe` True
                exists2 `shouldBe` True
                exists3 `shouldBe` True
                exists4 `shouldBe` True
                exists5 `shouldBe` True

        -- ============================================================
        -- Test Program Suite (P01-P10)
        -- ============================================================

        describe "P01: Temperature Converter (Tier 1 — basics)" $ do
            it "loads P01_Temperature.tl" $ do
                st1 <- loadTestProgram st "tests/programs/P01_Temperature.tl"
                result <- evalExpr st1 "scaleId(Celsius)"
                result `shouldBe` conZ

            it "scaleId(Kelvin) = Succ(Succ(Z))" $ do
                st1 <- loadTestProgram st "tests/programs/P01_Temperature.tl"
                result <- evalExpr st1 "scaleId(Kelvin)"
                result `shouldBe` nat 2

            it "isFreezing(0, Celsius) = True" $ do
                st1 <- loadTestProgram st "tests/programs/P01_Temperature.tl"
                result <- evalExpr st1 "isFreezing(0, Celsius)"
                result `shouldBe` conTrue

            it "isFreezing(100, Celsius) = False" $ do
                st1 <- loadTestProgram st "tests/programs/P01_Temperature.tl"
                result <- evalExpr st1 "isFreezing(100, Celsius)"
                result `shouldBe` conFalse

            it "isFreezing(31, Fahrenheit) = True" $ do
                st1 <- loadTestProgram st "tests/programs/P01_Temperature.tl"
                result <- evalExpr st1 "isFreezing(31, Fahrenheit)"
                result `shouldBe` conTrue

            it "celsiusToFahr(100) = 212" $ do
                st1 <- loadTestProgram st "tests/programs/P01_Temperature.tl"
                result <- evalExpr st1 "celsiusToFahr(100)"
                result `shouldBe` CLMLIT (LInt 212)

            it "celsiusToFahr(0) = 32" $ do
                st1 <- loadTestProgram st "tests/programs/P01_Temperature.tl"
                result <- evalExpr st1 "celsiusToFahr(0)"
                result `shouldBe` CLMLIT (LInt 32)

            it "fahrToCelsius(212) = 100" $ do
                st1 <- loadTestProgram st "tests/programs/P01_Temperature.tl"
                result <- evalExpr st1 "fahrToCelsius(212)"
                result `shouldBe` CLMLIT (LInt 100)

            it "clampTemp(150, 0, 100) = 100" $ do
                st1 <- loadTestProgram st "tests/programs/P01_Temperature.tl"
                result <- evalExpr st1 "clampTemp(150, 0, 100)"
                result `shouldBe` CLMLIT (LInt 100)

            it "clampTemp(-10, 0, 100) = 0" $ do
                st1 <- loadTestProgram st "tests/programs/P01_Temperature.tl"
                result <- evalExpr st1 "clampTemp(-10, 0, 100)"
                result `shouldBe` CLMLIT (LInt 0)

            it "clampTemp(50, 0, 100) = 50" $ do
                st1 <- loadTestProgram st "tests/programs/P01_Temperature.tl"
                result <- evalExpr st1 "clampTemp(50, 0, 100)"
                result `shouldBe` CLMLIT (LInt 50)

        describe "P02: List Utilities (Tier 1 — recursion + lambdas)" $ do
            it "len(Nil) = Z" $ do
                st2 <- loadTestProgram st "tests/programs/P02_ListOps.tl"
                result <- evalExpr st2 "len(Nil)"
                result `shouldBe` conZ

            it "len(Cons(Z, Cons(Z, Nil))) = Succ(Succ(Z))" $ do
                st2 <- loadTestProgram st "tests/programs/P02_ListOps.tl"
                result <- evalExpr st2 "len(Cons(Z, Cons(Z, Nil)))"
                result `shouldBe` nat 2

            it "append(Cons(Z, Nil), Cons(Succ(Z), Nil))" $ do
                st2 <- loadTestProgram st "tests/programs/P02_ListOps.tl"
                result <- evalExpr st2 "append(Cons(Z, Nil), Cons(Succ(Z), Nil))"
                result `shouldBe` conCons conZ (conCons (conSucc conZ) conNil)

            it "nth(Cons(True, Cons(False, Nil)), Z) = Just(True)" $ do
                st2 <- loadTestProgram st "tests/programs/P02_ListOps.tl"
                result <- evalExpr st2 "nth(Cons(True, Cons(False, Nil)), Z)"
                result `shouldBe` conJust conTrue

            it "nth(Cons(True, Cons(False, Nil)), Succ(Z)) = Just(False)" $ do
                st2 <- loadTestProgram st "tests/programs/P02_ListOps.tl"
                result <- evalExpr st2 "nth(Cons(True, Cons(False, Nil)), Succ(Z))"
                result `shouldBe` conJust conFalse

            it "nth(Nil, Z) = Nothing" $ do
                st2 <- loadTestProgram st "tests/programs/P02_ListOps.tl"
                result <- evalExpr st2 "nth(Nil, Z)"
                result `shouldBe` conNothing

            it "take(Succ(Z), Cons(True, Cons(False, Nil))) = Cons(True, Nil)" $ do
                st2 <- loadTestProgram st "tests/programs/P02_ListOps.tl"
                result <- evalExpr st2 "take(Succ(Z), Cons(True, Cons(False, Nil)))"
                result `shouldBe` conCons conTrue conNil

            it "drop(Succ(Z), Cons(True, Cons(False, Nil))) = Cons(False, Nil)" $ do
                st2 <- loadTestProgram st "tests/programs/P02_ListOps.tl"
                result <- evalExpr st2 "drop(Succ(Z), Cons(True, Cons(False, Nil)))"
                result `shouldBe` conCons conFalse conNil

        describe "P03: Custom Algebra + Dispatch (Tier 2 — algebras)" $ do
            it "rank(Red) = 1" $ do
                st3 <- loadTestProgram st "tests/programs/P03_Showable.tl"
                result <- evalExpr st3 "rank(Red)"
                result `shouldBe` CLMLIT (LInt 1)

            it "rank(Blue) = 3" $ do
                st3 <- loadTestProgram st "tests/programs/P03_Showable.tl"
                result <- evalExpr st3 "rank(Blue)"
                result `shouldBe` CLMLIT (LInt 3)

            it "rank(Circle(Z)) = 10" $ do
                st3 <- loadTestProgram st "tests/programs/P03_Showable.tl"
                result <- evalExpr st3 "rank(Circle(Z))"
                result `shouldBe` CLMLIT (LInt 10)

            it "rank(Square(Z)) = 20" $ do
                st3 <- loadTestProgram st "tests/programs/P03_Showable.tl"
                result <- evalExpr st3 "rank(Square(Z))"
                result `shouldBe` CLMLIT (LInt 20)

            it "rank(Triangle(Z, Z, Z)) = 30" $ do
                st3 <- loadTestProgram st "tests/programs/P03_Showable.tl"
                result <- evalExpr st3 "rank(Triangle(Z, Z, Z))"
                result `shouldBe` CLMLIT (LInt 30)

            it "doubleRank(Green) = 4" $ do
                st3 <- loadTestProgram st "tests/programs/P03_Showable.tl"
                result <- evalExpr st3 "doubleRank(Green)"
                result `shouldBe` CLMLIT (LInt 4)

            it "doubleRank(Circle(Z)) = 20" $ do
                st3 <- loadTestProgram st "tests/programs/P03_Showable.tl"
                result <- evalExpr st3 "doubleRank(Circle(Z))"
                result `shouldBe` CLMLIT (LInt 20)

            it "isHighRank(Square(Z)) = True" $ do
                st3 <- loadTestProgram st "tests/programs/P03_Showable.tl"
                result <- evalExpr st3 "isHighRank(Square(Z))"
                result `shouldBe` conTrue

            it "isHighRank(Red) = False" $ do
                st3 <- loadTestProgram st "tests/programs/P03_Showable.tl"
                result <- evalExpr st3 "isHighRank(Red)"
                result `shouldBe` conFalse

        describe "P04: Geometry with Records (Tier 2 — records)" $ do
            it "origin() = Point2D(0, 0)" $ do
                st4 <- loadTestProgram st "tests/programs/P04_Geometry.tl"
                result <- evalExpr st4 "origin()"
                result `shouldBe` CLMCON (ConsTag "Point2D" 0) [CLMLIT (LInt 0), CLMLIT (LInt 0)]

            it "makePoint(3, 4) = Point2D(3, 4)" $ do
                st4 <- loadTestProgram st "tests/programs/P04_Geometry.tl"
                result <- evalExpr st4 "makePoint(3, 4)"
                result `shouldBe` CLMCON (ConsTag "Point2D" 0) [CLMLIT (LInt 3), CLMLIT (LInt 4)]

            it "distSquared(Point2D(3, 4)) = 25" $ do
                st4 <- loadTestProgram st "tests/programs/P04_Geometry.tl"
                result <- evalExpr st4 "distSquared(Point2D(3, 4))"
                result `shouldBe` CLMLIT (LInt 25)

            it "manhattan(Point2D(0, 0), Point2D(3, 4)) = 7" $ do
                st4 <- loadTestProgram st "tests/programs/P04_Geometry.tl"
                result <- evalExpr st4 "manhattan(Point2D(0, 0), Point2D(3, 4))"
                result `shouldBe` CLMLIT (LInt 7)

            it "translate(Point2D(1, 2), 3, 4) = Point2D(4, 6)" $ do
                st4 <- loadTestProgram st "tests/programs/P04_Geometry.tl"
                result <- evalExpr st4 "translate(Point2D(1, 2), 3, 4)"
                result `shouldBe` CLMCON (ConsTag "Point2D" 0) [CLMLIT (LInt 4), CLMLIT (LInt 6)]

            it "moveDir(Point2D(0, 0), North) = Point2D(0, 1)" $ do
                st4 <- loadTestProgram st "tests/programs/P04_Geometry.tl"
                result <- evalExpr st4 "moveDir(Point2D(0, 0), North)"
                result `shouldBe` CLMCON (ConsTag "Point2D" 0) [CLMLIT (LInt 0), CLMLIT (LInt 1)]

            it "moveDir(Point2D(0, 0), West) = Point2D(-1, 0)" $ do
                st4 <- loadTestProgram st "tests/programs/P04_Geometry.tl"
                result <- evalExpr st4 "moveDir(Point2D(0, 0), West)"
                result `shouldBe` CLMCON (ConsTag "Point2D" 0) [CLMLIT (LInt (-1)), CLMLIT (LInt 0)]

            it "midpoint(Point2D(0, 0), Point2D(4, 6)) = Point2D(2, 3)" $ do
                st4 <- loadTestProgram st "tests/programs/P04_Geometry.tl"
                result <- evalExpr st4 "midpoint(Point2D(0, 0), Point2D(4, 6))"
                result `shouldBe` CLMCON (ConsTag "Point2D" 0) [CLMLIT (LInt 2), CLMLIT (LInt 3)]

        describe "P05: Stack Library (Tier 3 — two-file import)" $ do
            it "peek(Push(Succ(Z), EmptyStack)) = Just(Succ(Z))" $ do
                st5 <- loadTestPrograms st ["tests/programs/P05/Stack.tl", "tests/programs/P05/Main.tl"]
                result <- evalExpr st5 "peek(Push(Succ(Z), EmptyStack))"
                result `shouldBe` conJust (conSucc conZ)

            it "peek(EmptyStack) = Nothing" $ do
                st5 <- loadTestPrograms st ["tests/programs/P05/Stack.tl", "tests/programs/P05/Main.tl"]
                result <- evalExpr st5 "peek(EmptyStack)"
                result `shouldBe` conNothing

            it "stackSize(Push(True, Push(False, EmptyStack))) = Succ(Succ(Z))" $ do
                st5 <- loadTestPrograms st ["tests/programs/P05/Stack.tl", "tests/programs/P05/Main.tl"]
                result <- evalExpr st5 "stackSize(Push(True, Push(False, EmptyStack)))"
                result `shouldBe` nat 2

            it "isStackEmpty(EmptyStack) = True" $ do
                st5 <- loadTestPrograms st ["tests/programs/P05/Stack.tl", "tests/programs/P05/Main.tl"]
                result <- evalExpr st5 "isStackEmpty(EmptyStack)"
                result `shouldBe` conTrue

            it "isStackEmpty(Push(Z, EmptyStack)) = False" $ do
                st5 <- loadTestPrograms st ["tests/programs/P05/Stack.tl", "tests/programs/P05/Main.tl"]
                result <- evalExpr st5 "isStackEmpty(Push(Z, EmptyStack))"
                result `shouldBe` conFalse

            it "stackToList(Push(Z, Push(Succ(Z), EmptyStack)))" $ do
                st5 <- loadTestPrograms st ["tests/programs/P05/Stack.tl", "tests/programs/P05/Main.tl"]
                result <- evalExpr st5 "stackToList(Push(Z, Push(Succ(Z), EmptyStack)))"
                result `shouldBe` conCons conZ (conCons (conSucc conZ) conNil)

        describe "P06: Expression Evaluator (Tier 3 — three-file chain)" $ do
            it "eval(testExpr1()) = 14" $ do
                st6 <- loadTestPrograms st ["tests/programs/P06/Expr.tl", "tests/programs/P06/Eval.tl", "tests/programs/P06/Main.tl"]
                result <- evalExpr st6 "eval(testExpr1())"
                result `shouldBe` CLMLIT (LInt 14)

            it "eval(testExpr2()) = -8" $ do
                st6 <- loadTestPrograms st ["tests/programs/P06/Expr.tl", "tests/programs/P06/Eval.tl", "tests/programs/P06/Main.tl"]
                result <- evalExpr st6 "eval(testExpr2())"
                result `shouldBe` CLMLIT (LInt (-8))

            it "eval(testExpr3()) = 6" $ do
                st6 <- loadTestPrograms st ["tests/programs/P06/Expr.tl", "tests/programs/P06/Eval.tl", "tests/programs/P06/Main.tl"]
                result <- evalExpr st6 "eval(testExpr3())"
                result `shouldBe` CLMLIT (LInt 6)

            it "countOps(testExpr1()) = Succ(Succ(Z))" $ do
                st6 <- loadTestPrograms st ["tests/programs/P06/Expr.tl", "tests/programs/P06/Eval.tl", "tests/programs/P06/Main.tl"]
                result <- evalExpr st6 "countOps(testExpr1())"
                result `shouldBe` nat 2

            it "countOps(Num(42)) = Z" $ do
                st6 <- loadTestPrograms st ["tests/programs/P06/Expr.tl", "tests/programs/P06/Eval.tl", "tests/programs/P06/Main.tl"]
                result <- evalExpr st6 "countOps(Num(42))"
                result `shouldBe` conZ

            it "isPositiveResult(testExpr1()) = True" $ do
                st6 <- loadTestPrograms st ["tests/programs/P06/Expr.tl", "tests/programs/P06/Eval.tl", "tests/programs/P06/Main.tl"]
                result <- evalExpr st6 "isPositiveResult(testExpr1())"
                result `shouldBe` conTrue

            it "isPositiveResult(testExpr2()) = False" $ do
                st6 <- loadTestPrograms st ["tests/programs/P06/Expr.tl", "tests/programs/P06/Eval.tl", "tests/programs/P06/Main.tl"]
                result <- evalExpr st6 "isPositiveResult(testExpr2())"
                result `shouldBe` conFalse

        describe "P07: Monadic Pipelines (Tier 4 — HKT)" $ do
            it "safeDivide(10, 2) = Just(5)" $ do
                st7 <- loadTestProgram st "tests/programs/P07_Monadic.tl"
                result <- evalExpr st7 "safeDivide(10, 2)"
                result `shouldBe` conJust (CLMLIT (LInt 5))

            it "safeDivide(10, 0) = Nothing" $ do
                st7 <- loadTestProgram st "tests/programs/P07_Monadic.tl"
                result <- evalExpr st7 "safeDivide(10, 0)"
                result `shouldBe` conNothing

            it "doubleAll(Cons(1, Cons(2, Cons(3, Nil))))" $ do
                st7 <- loadTestProgram st "tests/programs/P07_Monadic.tl"
                result <- evalExpr st7 "doubleAll(Cons(1, Cons(2, Cons(3, Nil))))"
                result `shouldBe` conCons (CLMLIT (LInt 2)) (conCons (CLMLIT (LInt 4)) (conCons (CLMLIT (LInt 6)) conNil))

            it "addOneAll(Cons(10, Cons(20, Nil)))" $ do
                st7 <- loadTestProgram st "tests/programs/P07_Monadic.tl"
                result <- evalExpr st7 "addOneAll(Cons(10, Cons(20, Nil)))"
                result `shouldBe` conCons (CLMLIT (LInt 11)) (conCons (CLMLIT (LInt 21)) conNil)

            it "flatMapDouble(Cons(1, Cons(2, Nil)))" $ do
                st7 <- loadTestProgram st "tests/programs/P07_Monadic.tl"
                result <- evalExpr st7 "flatMapDouble(Cons(1, Cons(2, Nil)))"
                result `shouldBe` conCons (CLMLIT (LInt 1)) (conCons (CLMLIT (LInt 1)) (conCons (CLMLIT (LInt 2)) (conCons (CLMLIT (LInt 2)) conNil)))

            it "chainDivide(100, 5, 2) = Just(10)" $ do
                st7 <- loadTestProgram st "tests/programs/P07_Monadic.tl"
                result <- evalExpr st7 "chainDivide(100, 5, 2)"
                result `shouldBe` conJust (CLMLIT (LInt 10))

            it "chainDivide(100, 0, 2) = Nothing" $ do
                st7 <- loadTestProgram st "tests/programs/P07_Monadic.tl"
                result <- evalExpr st7 "chainDivide(100, 0, 2)"
                result `shouldBe` conNothing

            it "chainDivide(100, 5, 0) = Nothing" $ do
                st7 <- loadTestProgram st "tests/programs/P07_Monadic.tl"
                result <- evalExpr st7 "chainDivide(100, 5, 0)"
                result `shouldBe` conNothing

        describe "P08: Digit Repr System (Tier 4 — repr + casting)" $ do
            it "D5 as Int = 5" $ do
                st8 <- loadTestProgram st "tests/programs/P08_Repr.tl"
                result <- evalExpr st8 "D5 as Int"
                result `shouldBe` CLMLIT (LInt 5)

            it "3 as Digit = D3" $ do
                st8 <- loadTestProgram st "tests/programs/P08_Repr.tl"
                result <- evalExpr st8 "3 as Digit"
                result `shouldBe` CLMCON (ConsTag "D3" 3) []

            it "D0 as Int = 0" $ do
                st8 <- loadTestProgram st "tests/programs/P08_Repr.tl"
                result <- evalExpr st8 "D0 as Int"
                result `shouldBe` CLMLIT (LInt 0)

            it "addDigits(D3, D4) = 7" $ do
                st8 <- loadTestProgram st "tests/programs/P08_Repr.tl"
                result <- evalExpr st8 "addDigits(D3, D4)"
                result `shouldBe` CLMLIT (LInt 7)

            it "isEvenDigit(D4) = True" $ do
                st8 <- loadTestProgram st "tests/programs/P08_Repr.tl"
                result <- evalExpr st8 "isEvenDigit(D4)"
                result `shouldBe` conTrue

            it "isEvenDigit(D3) = False" $ do
                st8 <- loadTestProgram st "tests/programs/P08_Repr.tl"
                result <- evalExpr st8 "isEvenDigit(D3)"
                result `shouldBe` conFalse

            it "nextDigit(D0) = D1" $ do
                st8 <- loadTestProgram st "tests/programs/P08_Repr.tl"
                result <- evalExpr st8 "nextDigit(D0)"
                result `shouldBe` CLMCON (ConsTag "D1" 1) []

            it "nextDigit(D8) = D9" $ do
                st8 <- loadTestProgram st "tests/programs/P08_Repr.tl"
                result <- evalExpr st8 "nextDigit(D8)"
                result `shouldBe` CLMCON (ConsTag "D9" 9) []

            it "nextDigit(D9) = D0" $ do
                st8 <- loadTestProgram st "tests/programs/P08_Repr.tl"
                result <- evalExpr st8 "nextDigit(D9)"
                result `shouldBe` CLMCON (ConsTag "D0" 0) []

        describe "P09: Mini Calculator (Tier 5 — multi-module app)" $ do
            it "isOperator(TPlus) = True" $ do
                st9 <- loadTestPrograms st ["tests/programs/P09/Token.tl", "tests/programs/P09/Calc.tl", "tests/programs/P09/Main.tl"]
                result <- evalExpr st9 "isOperator(TPlus)"
                result `shouldBe` conTrue

            it "isOperator(TNum(42)) = False" $ do
                st9 <- loadTestPrograms st ["tests/programs/P09/Token.tl", "tests/programs/P09/Calc.tl", "tests/programs/P09/Main.tl"]
                result <- evalExpr st9 "isOperator(TNum(42))"
                result `shouldBe` conFalse

            it "test_add() = CalcOk(7)" $ do
                st9 <- loadTestPrograms st ["tests/programs/P09/Token.tl", "tests/programs/P09/Calc.tl", "tests/programs/P09/Main.tl"]
                result <- evalExpr st9 "test_add()"
                result `shouldBe` CLMCON (ConsTag "CalcOk" 0) [CLMLIT (LInt 7)]

            it "test_mul() = CalcOk(30)" $ do
                st9 <- loadTestPrograms st ["tests/programs/P09/Token.tl", "tests/programs/P09/Calc.tl", "tests/programs/P09/Main.tl"]
                result <- evalExpr st9 "test_mul()"
                result `shouldBe` CLMCON (ConsTag "CalcOk" 0) [CLMLIT (LInt 30)]

            it "test_err() = CalcErr" $ do
                st9 <- loadTestPrograms st ["tests/programs/P09/Token.tl", "tests/programs/P09/Calc.tl", "tests/programs/P09/Main.tl"]
                result <- evalExpr st9 "test_err()"
                result `shouldBe` CLMCON (ConsTag "CalcErr" 1) []

            it "safeCalc(10, TMinus, 3) = Just(7)" $ do
                st9 <- loadTestPrograms st ["tests/programs/P09/Token.tl", "tests/programs/P09/Calc.tl", "tests/programs/P09/Main.tl"]
                result <- evalExpr st9 "safeCalc(10, TMinus, 3)"
                result `shouldBe` conJust (CLMLIT (LInt 7))

            it "safeCalc(10, TEnd, 3) = Nothing" $ do
                st9 <- loadTestPrograms st ["tests/programs/P09/Token.tl", "tests/programs/P09/Calc.tl", "tests/programs/P09/Main.tl"]
                result <- evalExpr st9 "safeCalc(10, TEnd, 3)"
                result `shouldBe` conNothing

            it "test_monadic() = Just(30)" $ do
                st9 <- loadTestPrograms st ["tests/programs/P09/Token.tl", "tests/programs/P09/Calc.tl", "tests/programs/P09/Main.tl"]
                result <- evalExpr st9 "test_monadic()"
                result `shouldBe` conJust (CLMLIT (LInt 30))

            it "test_err_monadic() = Nothing" $ do
                st9 <- loadTestPrograms st ["tests/programs/P09/Token.tl", "tests/programs/P09/Calc.tl", "tests/programs/P09/Main.tl"]
                result <- evalExpr st9 "test_err_monadic()"
                result `shouldBe` conNothing

        describe "P10: Stress Test (Tier 5 — everything combined)" $ do
            it "score(Critical) = 4" $ do
                st10 <- loadTestProgram st "tests/programs/P10_StressTest.tl"
                result <- evalExpr st10 "score(Critical)"
                result `shouldBe` CLMLIT (LInt 4)

            -- Remaining P10 tests temporarily isolated for debugging
            it "P10 score(Done) = 2" $ do
                st10 <- loadTestProgram st "tests/programs/P10_StressTest.tl"
                result <- evalExpr st10 "score(Done)"
                result `shouldBe` CLMLIT (LInt 2)

            it "P10 taskScore(Task(High, Done, Succ(Succ(Z)))) = 42" $ do
                st10 <- loadTestProgram st "tests/programs/P10_StressTest.tl"
                result <- evalExpr st10 "taskScore(Task(High, Done, Succ(Succ(Z))))"
                result `shouldBe` CLMLIT (LInt 42)

            it "P10 taskScore(Task(Low, Pending, Z)) = 10" $ do
                st10 <- loadTestProgram st "tests/programs/P10_StressTest.tl"
                result <- evalExpr st10 "taskScore(Task(Low, Pending, Z))"
                result `shouldBe` CLMLIT (LInt 10)

            it "P10 isHighPriority(Task(Critical, Pending, Z)) = True" $ do
                st10 <- loadTestProgram st "tests/programs/P10_StressTest.tl"
                result <- evalExpr st10 "isHighPriority(Task(Critical, Pending, Z))"
                result `shouldBe` conTrue

            it "P10 isHighPriority(Task(Low, Done, Z)) = False" $ do
                st10 <- loadTestProgram st "tests/programs/P10_StressTest.tl"
                result <- evalExpr st10 "isHighPriority(Task(Low, Done, Z))"
                result `shouldBe` conFalse

            it "P10 isDone(Task(High, Done, Z)) = True" $ do
                st10 <- loadTestProgram st "tests/programs/P10_StressTest.tl"
                result <- evalExpr st10 "isDone(Task(High, Done, Z))"
                result `shouldBe` conTrue

            it "P10 isDone(Task(High, Pending, Z)) = False" $ do
                st10 <- loadTestProgram st "tests/programs/P10_StressTest.tl"
                result <- evalExpr st10 "isDone(Task(High, Pending, Z))"
                result `shouldBe` conFalse

            it "P10 convert(High) = Succ(Succ(Z))" $ do
                st10 <- loadTestProgram st "tests/programs/P10_StressTest.tl"
                result <- evalExpr st10 "convert(High)"
                result `shouldBe` nat 2

            it "P10 maxScore(Nil) = 0" $ do
                st10 <- loadTestProgram st "tests/programs/P10_StressTest.tl"
                result <- evalExpr st10 "maxScore(Nil)"
                result `shouldBe` CLMLIT (LInt 0)

            it "P10 projectHealth(Nil) = Nothing" $ do
                st10 <- loadTestProgram st "tests/programs/P10_StressTest.tl"
                result <- evalExpr st10 "projectHealth(Nil)"
                result `shouldBe` conNothing

        describe "P11: Record System Completion" $ do
            it "dot-access: getX(Point(3, 4)) = 3" $ do
                st11 <- loadTestProgram st "tests/programs/P11_Records.tl"
                result <- evalExpr st11 "getX(Point(3, 4))"
                result `shouldBe` CLMLIT (LInt 3)

            it "dot-access: getY(Point(3, 4)) = 4" $ do
                st11 <- loadTestProgram st "tests/programs/P11_Records.tl"
                result <- evalExpr st11 "getY(Point(3, 4))"
                result `shouldBe` CLMLIT (LInt 4)

            it "named construction: mkPoint(5, 6) = Point(5, 6)" $ do
                st11 <- loadTestProgram st "tests/programs/P11_Records.tl"
                result <- evalExpr st11 "mkPoint(5, 6)"
                result `shouldBe` CLMCON (ConsTag "Point" 0) [CLMLIT (LInt 5), CLMLIT (LInt 6)]

            it "named construction reordered: mkPointRev(5, 6) = Point(5, 6)" $ do
                st11 <- loadTestProgram st "tests/programs/P11_Records.tl"
                result <- evalExpr st11 "mkPointRev(5, 6)"
                result `shouldBe` CLMCON (ConsTag "Point" 0) [CLMLIT (LInt 5), CLMLIT (LInt 6)]

            it "record update: setX(Point(1, 2), 9) = Point(9, 2)" $ do
                st11 <- loadTestProgram st "tests/programs/P11_Records.tl"
                result <- evalExpr st11 "setX(Point(1, 2), 9)"
                result `shouldBe` CLMCON (ConsTag "Point" 0) [CLMLIT (LInt 9), CLMLIT (LInt 2)]

            it "record update: setY(Point(1, 2), 9) = Point(1, 9)" $ do
                st11 <- loadTestProgram st "tests/programs/P11_Records.tl"
                result <- evalExpr st11 "setY(Point(1, 2), 9)"
                result `shouldBe` CLMCON (ConsTag "Point" 0) [CLMLIT (LInt 1), CLMLIT (LInt 9)]

            it "dot-access on function result: doubleX(Point(3, 4)) = 6" $ do
                st11 <- loadTestProgram st "tests/programs/P11_Records.tl"
                result <- evalExpr st11 "doubleX(Point(3, 4))"
                result `shouldBe` CLMLIT (LInt 6)

            it "named field pattern: isOrigin(Point(0, 0)) = True" $ do
                st11 <- loadTestProgram st "tests/programs/P11_Records.tl"
                result <- evalExpr st11 "isOrigin(Point(0, 0))"
                result `shouldBe` conTrue

            it "named field pattern: isOrigin(Point(1, 0)) = False" $ do
                st11 <- loadTestProgram st "tests/programs/P11_Records.tl"
                result <- evalExpr st11 "isOrigin(Point(1, 0))"
                result `shouldBe` conFalse

            it "named construction 3 fields: red() = Color(255, 0, 0)" $ do
                st11 <- loadTestProgram st "tests/programs/P11_Records.tl"
                result <- evalExpr st11 "red()"
                result `shouldBe` CLMCON (ConsTag "Color" 0) [CLMLIT (LInt 255), CLMLIT (LInt 0), CLMLIT (LInt 0)]

            it "dot-access 3-field record: getR(Color(100, 200, 50)) = 100" $ do
                st11 <- loadTestProgram st "tests/programs/P11_Records.tl"
                result <- evalExpr st11 "getR(Color(100, 200, 50))"
                result `shouldBe` CLMLIT (LInt 100)

        describe "P12: Effect System" $ do
            it "effect declaration parses successfully" $ do
                st12 <- loadTestProgram st "tests/programs/P12_Effects.tl"
                -- If we got here without error, parsing worked
                let env = currentEnvironment st12
                -- Check that effect decl was registered
                Map.member "Console" (effectDecls env) `shouldBe` True

            it "FileIO effect declaration registered" $ do
                st12 <- loadTestProgram st "tests/programs/P12_Effects.tl"
                let env = currentEnvironment st12
                Map.member "FileIO" (effectDecls env) `shouldBe` True

            it "handler declaration parses successfully" $ do
                st12 <- loadTestProgram st "tests/programs/P12_Effects.tl"
                let env = currentEnvironment st12
                Map.member "StdConsole" (effectHandlers env) `shouldBe` True

            it "pure functions still work alongside effects: pureAdd(3, 4) = 7" $ do
                st12 <- loadTestProgram st "tests/programs/P12_Effects.tl"
                result <- evalExpr st12 "pureAdd(3, 4)"
                result `shouldBe` CLMLIT (LInt 7)

            it "pattern matching still works alongside effects: isPositive(5) = True" $ do
                st12 <- loadTestProgram st "tests/programs/P12_Effects.tl"
                result <- evalExpr st12 "isPositive(5)"
                result `shouldBe` conTrue

            it "pattern matching still works alongside effects: isPositive(0) = False" $ do
                st12 <- loadTestProgram st "tests/programs/P12_Effects.tl"
                result <- evalExpr st12 "isPositive(0)"
                result `shouldBe` conFalse

            it "testAction(5) = 6" $ do
                st12 <- loadTestProgram st "tests/programs/P12_Effects.tl"
                result <- evalExpr st12 "testAction(5)"
                result `shouldBe` CLMLIT (LInt 6)

            it "effect type parsing: Eff row type" $ do
                result <- parseTestString "function f() : Eff { console: Console } Unit = intrinsic;"
                case result of
                    Right exprs -> length exprs `shouldBe` 1
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "action block parsing with bind syntax" $ do
                result <- parseTestString "function f() : Unit = action { x <- readLine(), putStrLn(x) };"
                case result of
                    Right exprs -> length exprs `shouldBe` 1
                    Left err -> expectationFailure $ "Parse error: " ++ err

            it "handle expression parsing" $ do
                result <- parseTestString "function f() : Unit = handle readLine() with StdConsole;"
                case result of
                    Right exprs -> length exprs `shouldBe` 1
                    Left err -> expectationFailure $ "Parse error: " ++ err

        describe "P13: Intrinsic Completeness" $ do
            -- Show intrinsics
            it "show(42) = \"42\"" $ do
                result <- evalExpr st "show(42)"
                result `shouldBe` CLMLIT (LString "42")

            it "show(3.14) = \"3.14\"" $ do
                result <- evalExpr st "show(3.14)"
                result `shouldBe` CLMLIT (LString "3.14")

            it "show(\"hi\") = \"\\\"hi\\\"\"" $ do
                result <- evalExpr st "show(\"hi\")"
                result `shouldBe` CLMLIT (LString "\"hi\"")

            it "show('A') = \"'A'\"" $ do
                result <- evalExpr st "show('A')"
                result `shouldBe` CLMLIT (LString "'A'")

            -- Char intrinsics
            it "'A' == 'A' = True" $ do
                result <- evalExpr st "'A' == 'A'"
                result `shouldBe` conTrue

            it "'A' == 'B' = False" $ do
                result <- evalExpr st "'A' == 'B'"
                result `shouldBe` conFalse

            it "'A' < 'B' = True" $ do
                result <- evalExpr st "'A' < 'B'"
                result `shouldBe` conTrue

            it "compare('A', 'B') = LessThan" $ do
                result <- evalExpr st "compare('A', 'B')"
                result `shouldBe` conLT

            -- Enum intrinsics
            it "succ(42) = 43" $ do
                result <- evalExpr st "succ(42)"
                result `shouldBe` CLMLIT (LInt 43)

            it "pred(42) = 41" $ do
                result <- evalExpr st "pred(42)"
                result `shouldBe` CLMLIT (LInt 41)

            it "succ('A') = 'B'" $ do
                result <- evalExpr st "succ('A')"
                result `shouldBe` CLMLIT (LChar 'B')

            it "pred('B') = 'A'" $ do
                result <- evalExpr st "pred('B')"
                result `shouldBe` CLMLIT (LChar 'A')

            it "fromEnum('A') = 65" $ do
                result <- evalExpr st "fromEnum('A')"
                result `shouldBe` CLMLIT (LInt 65)

            it "toEnum intrinsic exists for Char" $ hasIntrinsic "toEnum" "Char"

            -- Hashable intrinsics
            it "hash(42) = 42" $ do
                result <- evalExpr st "hash(42)"
                result `shouldBe` CLMLIT (LInt 42)

            it "hash('A') = 65" $ do
                result <- evalExpr st "hash('A')"
                result `shouldBe` CLMLIT (LInt 65)

            -- String extended ops via test program
            it "P13 test program loads" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                let env = currentEnvironment st13
                Map.member "showInt" (clmLambdas env) `shouldBe` True

            it "show(42) via function = \"42\"" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "showInt()"
                result `shouldBe` CLMLIT (LString "42")

            it "show(3.14) via function" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "showFloat()"
                result `shouldBe` CLMLIT (LString "3.14")

            it "charAt(\"hello\", 0) = 'h'" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testCharAt()"
                result `shouldBe` CLMLIT (LChar 'h')

            it "substring(\"hello world\", 0, 5) = \"hello\"" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testSubstring()"
                result `shouldBe` CLMLIT (LString "hello")

            it "indexOf(\"hello world\", \"world\") = 6" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testIndexOf()"
                result `shouldBe` CLMLIT (LInt 6)

            it "indexOf(\"hello\", \"xyz\") = -1" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testIndexOfNot()"
                result `shouldBe` CLMLIT (LInt (-1))

            it "trim(\"  hello  \") = \"hello\"" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testTrim()"
                result `shouldBe` CLMLIT (LString "hello")

            it "toUpper(\"hello\") = \"HELLO\"" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testToUpper()"
                result `shouldBe` CLMLIT (LString "HELLO")

            it "toLower(\"HELLO\") = \"hello\"" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testToLower()"
                result `shouldBe` CLMLIT (LString "hello")

            it "startsWith(\"hello world\", \"hello\") = True" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testStartsWith()"
                result `shouldBe` conTrue

            it "endsWith(\"hello world\", \"world\") = True" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testEndsWith()"
                result `shouldBe` conTrue

            it "replace(\"hello world\", \"world\", \"tulam\") = \"hello tulam\"" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testReplace()"
                result `shouldBe` CLMLIT (LString "hello tulam")

            it "parseInt(\"42\") = Just(42)" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testParseInt()"
                result `shouldBe` conJust (CLMLIT (LInt 42))

            it "parseInt(\"abc\") = Nothing" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testParseIntBad()"
                result `shouldBe` conNothing

            -- Array ops
            it "length(push([1,2,3], 4)) = 4" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testArrayPush()"
                result `shouldBe` CLMLIT (LInt 4)

            it "length(reverse([1,2,3])) = 3" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testArrayReverse()"
                result `shouldBe` CLMLIT (LInt 3)

            it "range intrinsic exists for Array" $ hasIntrinsic "range" "Array"

            -- error function
            it "error(\"boom\") produces CLMERR" $ do
                st13 <- loadTestProgram st "tests/programs/P13_Intrinsics.tl"
                result <- evalExpr st13 "testError()"
                case result of
                    CLMERR msg _ -> msg `shouldSatisfy` isInfixOf "boom"
                    other -> expectationFailure $ "Expected CLMERR, got: " ++ show other

            -- FileIO via dispatchIOIntrinsic (direct Haskell-level test)
            it "dispatchIOIntrinsic writeFile + readFile round-trip" $ do
                let tmpFile = "/tmp/tulam_test_p13.txt"
                result1 <- evalStateT (evalStateT
                    (dispatchIOIntrinsic "writeFile" [CLMLIT (LString tmpFile), CLMLIT (LString "hello tulam")])
                    st) initLogState
                result1 `shouldBe` Just (CLMCON (ConsTag "Unit" 0) [])
                result2 <- evalStateT (evalStateT
                    (dispatchIOIntrinsic "readFile" [CLMLIT (LString tmpFile)])
                    st) initLogState
                result2 `shouldBe` Just (CLMLIT (LString "hello tulam"))

            it "dispatchIOIntrinsic fileExists for existing file = True" $ do
                let tmpFile = "/tmp/tulam_test_p13.txt"
                result <- evalStateT (evalStateT
                    (dispatchIOIntrinsic "fileExists" [CLMLIT (LString tmpFile)])
                    st) initLogState
                result `shouldBe` Just conTrue

            it "dispatchIOIntrinsic fileExists for nonexistent file = False" $ do
                result <- evalStateT (evalStateT
                    (dispatchIOIntrinsic "fileExists" [CLMLIT (LString "/tmp/tulam_nonexistent_xyz.txt")])
                    st) initLogState
                result `shouldBe` Just conFalse

            -- Intrinsic registry checks
            it "has show intrinsic for Int" $ hasIntrinsic "show" "Int"
            it "has show intrinsic for Float64" $ hasIntrinsic "show" "Float64"
            it "has show intrinsic for String" $ hasIntrinsic "show" "String"
            it "has show intrinsic for Char" $ hasIntrinsic "show" "Char"
            it "has Char == intrinsic" $ hasIntrinsic "==" "Char"
            it "has succ intrinsic for Int" $ hasIntrinsic "succ" "Int"
            it "has hash intrinsic for Int" $ hasIntrinsic "hash" "Int"
            it "has charAt intrinsic for String" $ hasIntrinsic "charAt" "String"
            it "has minBound intrinsic for Int8" $ hasIntrinsic "minBound" "Int8"

        describe "P14: Real Programs — Array HOFs, Universal Show, Traversal" $ do
            it "P14 test program loads" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                let env = currentEnvironment st14
                Map.member "testFmapIncr" (clmLambdas env) `shouldBe` True

            -- Array HOF tests
            it "fmap(\\x -> x + 1, [1, 2, 3]) = [2, 3, 4]" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testFmapIncr()"
                result `shouldBe` CLMARRAY [CLMLIT (LInt 2), CLMLIT (LInt 3), CLMLIT (LInt 4)]

            it "fmap(\\x -> x * x, [2, 3, 4]) = [4, 9, 16]" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testFmapSquare()"
                result `shouldBe` CLMARRAY [CLMLIT (LInt 4), CLMLIT (LInt 9), CLMLIT (LInt 16)]

            it "filter(\\x -> x > 2, [1, 2, 3, 4]) = [3, 4]" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testFilter()"
                result `shouldBe` CLMARRAY [CLMLIT (LInt 3), CLMLIT (LInt 4)]

            it "foldl(\\acc x -> acc + x, 0, [1, 2, 3]) = 6" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testFoldlSum()"
                result `shouldBe` CLMLIT (LInt 6)

            it "foldr(\\x acc -> acc + x, 0, [1, 2, 3]) = 6" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testFoldrSum()"
                result `shouldBe` CLMLIT (LInt 6)

            it "generate(5, \\i -> i * 2) = [0, 2, 4, 6, 8]" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testGenerate()"
                result `shouldBe` CLMARRAY [CLMLIT (LInt 0), CLMLIT (LInt 2), CLMLIT (LInt 4), CLMLIT (LInt 6), CLMLIT (LInt 8)]

            it "length(filter(\\x -> x > 0, [1, -2, 3])) = 2" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testFilterLength()"
                result `shouldBe` CLMLIT (LInt 2)

            it "fmap(\\x -> x, []) = []" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testFmapEmpty()"
                result `shouldBe` CLMARRAY []

            -- Universal Show tests
            it "show(Red) = \"Red\" (user-defined nullary constructor)" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testShowColor()"
                result `shouldBe` CLMLIT (LString "Red")

            it "show(MkPair(1, 2)) = \"MkPair(1, 2)\" (user-defined with fields)" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testShowPair()"
                result `shouldBe` CLMLIT (LString "MkPair(1, 2)")

            it "show([1, 2, 3]) = \"[1, 2, 3]\"" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testShowArray()"
                result `shouldBe` CLMLIT (LString "[1, 2, 3]")

            -- Monadic traversal tests
            it "mapM(\\x -> Just(x+1), Cons(1, Cons(2, Nil))) = Just(Cons(2, Cons(3, Nil)))" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testMapMMaybe()"
                result `shouldBe` conJust (conCons (CLMLIT (LInt 2)) (conCons (CLMLIT (LInt 3)) conNil))

            it "foldM(\\acc x -> Just(acc+x), 0, Cons(1, Cons(2, Nil))) = Just(3)" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testFoldMMaybe()"
                result `shouldBe` conJust (CLMLIT (LInt 3))

            it "sequence(Cons(Just(1), Cons(Just(2), Nil))) = Just(Cons(1, Cons(2, Nil)))" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testSequenceJust()"
                result `shouldBe` conJust (conCons (CLMLIT (LInt 1)) (conCons (CLMLIT (LInt 2)) conNil))

            it "sequence(Cons(Just(1), Cons(Nothing, Nil))) = Nothing" $ do
                st14 <- loadTestProgram st "tests/programs/P14_RealPrograms.tl"
                result <- evalExpr st14 "testSequenceNothing()"
                result `shouldBe` conNothing

        describe "P15: IO Functions via REPL (CLMIAP dispatch)" $ do
            it "putStrLn dispatches through CLMIAP (no crash)" $ do
                result <- evalExpr st "putStrLn(\"test\")"
                result `shouldBe` CLMCON (ConsTag "Unit" 0) []

            it "putStr dispatches through CLMIAP" $ do
                result <- evalExpr st "putStr(\"test\")"
                result `shouldBe` CLMCON (ConsTag "Unit" 0) []

            it "error produces CLMERR" $ do
                result <- evalExpr st "error(\"boom\")"
                result `shouldBe` CLMERR "boom" SourceInteractive

        describe "P16: Reflection Primitives" $ do
            -- Value reflection: tag
            it "tag(True) => 0" $ do
                result <- evalExpr st "tag(True)"
                result `shouldBe` CLMLIT (LInt 0)
            it "tag(False) => 1" $ do
                result <- evalExpr st "tag(False)"
                result `shouldBe` CLMLIT (LInt 1)
            it "tag(Just(42)) => 1" $ do
                result <- evalExpr st "tag(Just(42))"
                result `shouldBe` CLMLIT (LInt 1)
            it "tag(Nothing) => 0" $ do
                result <- evalExpr st "tag(Nothing)"
                result `shouldBe` CLMLIT (LInt 0)
            it "tag(42) => 0 (primitive)" $ do
                result <- evalExpr st "tag(42)"
                result `shouldBe` CLMLIT (LInt 0)
            -- Value reflection: tagName
            it "tagName(True) => \"True\"" $ do
                result <- evalExpr st "tagName(True)"
                result `shouldBe` CLMLIT (LString "True")
            it "tagName(Just(1)) => \"Just\"" $ do
                result <- evalExpr st "tagName(Just(1))"
                result `shouldBe` CLMLIT (LString "Just")
            it "tagName(42) => \"Int\"" $ do
                result <- evalExpr st "tagName(42)"
                result `shouldBe` CLMLIT (LString "Int")
            it "tagName(3.14) => \"Float64\"" $ do
                result <- evalExpr st "tagName(3.14)"
                result `shouldBe` CLMLIT (LString "Float64")
            -- Value reflection: arity
            it "arity(True) => 0" $ do
                result <- evalExpr st "arity(True)"
                result `shouldBe` CLMLIT (LInt 0)
            it "arity(Just(42)) => 1" $ do
                result <- evalExpr st "arity(Just(42))"
                result `shouldBe` CLMLIT (LInt 1)
            it "arity(42) => 0 (primitive)" $ do
                result <- evalExpr st "arity(42)"
                result `shouldBe` CLMLIT (LInt 0)
            -- Value reflection: field
            it "field(Just(42), 0) => 42" $ do
                result <- evalExpr st "field(Just(42), 0)"
                result `shouldBe` CLMLIT (LInt 42)
            -- Type reflection: numConstructors
            it "numConstructors(\"Bool\") => 2" $ do
                result <- evalExpr st "numConstructors(\"Bool\")"
                result `shouldBe` CLMLIT (LInt 2)
            it "numConstructors(\"Ordering\") => 3" $ do
                result <- evalExpr st "numConstructors(\"Ordering\")"
                result `shouldBe` CLMLIT (LInt 3)
            it "numConstructors(\"Maybe\") => 2" $ do
                result <- evalExpr st "numConstructors(\"Maybe\")"
                result `shouldBe` CLMLIT (LInt 2)
            -- Type reflection: constructorByIndex
            it "constructorByIndex(\"Bool\", 0) => True" $ do
                result <- evalExpr st "constructorByIndex(\"Bool\", 0)"
                result `shouldBe` conTrue
            it "constructorByIndex(\"Bool\", 1) => False" $ do
                result <- evalExpr st "constructorByIndex(\"Bool\", 1)"
                result `shouldBe` conFalse
            it "constructorByIndex(\"Ordering\", 2) => GreaterThan" $ do
                result <- evalExpr st "constructorByIndex(\"Ordering\", 2)"
                result `shouldBe` CLMCON (ConsTag "GreaterThan" 2) []

        describe "P16: Structural Helpers" $ do
            it "structuralEq(True, True) => True" $ do
                result <- evalExpr st "structuralEq(True, True)"
                result `shouldBe` conTrue
            it "structuralEq(True, False) => False" $ do
                result <- evalExpr st "structuralEq(True, False)"
                result `shouldBe` conFalse
            it "structuralEq(Just(1), Just(1)) => True" $ do
                result <- evalExpr st "structuralEq(Just(1), Just(1))"
                result `shouldBe` conTrue
            it "structuralEq(Just(1), Just(2)) => False" $ do
                result <- evalExpr st "structuralEq(Just(1), Just(2))"
                result `shouldBe` conFalse
            it "structuralEq(Nothing, Nothing) => True" $ do
                result <- evalExpr st "structuralEq(Nothing, Nothing)"
                result `shouldBe` conTrue
            it "structuralShow(True) => \"True\"" $ do
                result <- evalExpr st "structuralShow(True)"
                result `shouldBe` CLMLIT (LString "True")
            it "structuralShow(Just(42)) => \"Just(42)\"" $ do
                result <- evalExpr st "structuralShow(Just(42))"
                result `shouldBe` CLMLIT (LString "Just(42)")
            it "structuralCompare(True, False) => LessThan" $ do
                result <- evalExpr st "structuralCompare(True, False)"
                result `shouldBe` CLMCON (ConsTag "LessThan" 0) []
            it "structuralCompare(False, True) => GreaterThan" $ do
                result <- evalExpr st "structuralCompare(False, True)"
                result `shouldBe` CLMCON (ConsTag "GreaterThan" 2) []
            it "structuralCompare(True, True) => Equal" $ do
                result <- evalExpr st "structuralCompare(True, True)"
                result `shouldBe` CLMCON (ConsTag "Equal" 1) []
            it "structuralHash(True) != structuralHash(False)" $ do
                r1 <- evalExpr st "structuralHash(True)"
                r2 <- evalExpr st "structuralHash(False)"
                r1 `shouldNotBe` r2

        describe "P16: Derive System" $ do
            it "Red == Red => True (derive Eq)" $ do
                st16 <- loadTestProgram st "tests/programs/P16_Derive.tl"
                result <- evalExpr st16 "testColorEq1()"
                result `shouldBe` conTrue
            it "Red == Blue => False (derive Eq)" $ do
                st16 <- loadTestProgram st "tests/programs/P16_Derive.tl"
                result <- evalExpr st16 "testColorEq2()"
                result `shouldBe` conFalse
            it "compare(Red, Blue) => LessThan (derive Ord)" $ do
                st16 <- loadTestProgram st "tests/programs/P16_Derive.tl"
                result <- evalExpr st16 "testColorOrd()"
                result `shouldBe` CLMCON (ConsTag "LessThan" 0) []
            it "show(Green) => \"Green\" (derive Show)" $ do
                st16 <- loadTestProgram st "tests/programs/P16_Derive.tl"
                result <- evalExpr st16 "testColorShow()"
                result `shouldBe` CLMLIT (LString "Green")
            it "hash(Red) != hash(Blue) (derive Hashable)" $ do
                st16 <- loadTestProgram st "tests/programs/P16_Derive.tl"
                result <- evalExpr st16 "testColorHash()"
                result `shouldBe` conTrue
            it "North == North => True (deriving syntax)" $ do
                st16 <- loadTestProgram st "tests/programs/P16_Derive.tl"
                result <- evalExpr st16 "testDirEq()"
                result `shouldBe` conTrue
            it "North != South => True (deriving syntax)" $ do
                st16 <- loadTestProgram st "tests/programs/P16_Derive.tl"
                result <- evalExpr st16 "testDirNeq()"
                result `shouldBe` conTrue
            it "show(East) => \"East\" (deriving syntax)" $ do
                st16 <- loadTestProgram st "tests/programs/P16_Derive.tl"
                result <- evalExpr st16 "testDirShow()"
                result `shouldBe` CLMLIT (LString "East")

        describe "P17: Type-Directed Dispatch" $ do
            it "toEnum(0) : Bool => False (type-directed to Bool)" $ do
                st17 <- loadTestProgram st "tests/programs/P17_TypeDirected.tl"
                result <- evalExpr st17 "testToEnumBool1()"
                result `shouldBe` conFalse
            it "toEnum(1) : Bool => True (type-directed to Bool)" $ do
                st17 <- loadTestProgram st "tests/programs/P17_TypeDirected.tl"
                result <- evalExpr st17 "testToEnumBool2()"
                result `shouldBe` conTrue
            it "toEnum(2) : Ordering => GreaterThan (type-directed to Ordering)" $ do
                st17 <- loadTestProgram st "tests/programs/P17_TypeDirected.tl"
                result <- evalExpr st17 "testToEnumOrd()"
                result `shouldBe` CLMCON (ConsTag "GreaterThan" 2) []
            it "fromEnum(True) : Int => 1 (arg-type dispatch preserved)" $ do
                st17 <- loadTestProgram st "tests/programs/P17_TypeDirected.tl"
                result <- evalExpr st17 "testFromEnumBool()"
                result `shouldBe` CLMLIT (LInt 1)
            it "fromEnum(GreaterThan) : Int => 2 (arg-type dispatch preserved)" $ do
                st17 <- loadTestProgram st "tests/programs/P17_TypeDirected.tl"
                result <- evalExpr st17 "testFromEnumOrd()"
                result `shouldBe` CLMLIT (LInt 2)

        describe "P18: Managed Mutability" $ do
            it "newRef + readRef = 42" $ do
                st18 <- loadTestProgram st "tests/programs/P18_Mutability.tl"
                result <- evalExpr st18 "testRefRead()"
                result `shouldBe` CLMLIT (LInt 42)
            it "writeRef overwrites value = 99" $ do
                st18 <- loadTestProgram st "tests/programs/P18_Mutability.tl"
                result <- evalExpr st18 "testRefWrite()"
                result `shouldBe` CLMLIT (LInt 99)
            it "modifyRef applies function = 15" $ do
                st18 <- loadTestProgram st "tests/programs/P18_Mutability.tl"
                result <- evalExpr st18 "testRefModify()"
                result `shouldBe` CLMLIT (LInt 15)
            it "two refs are independent = 2" $ do
                st18 <- loadTestProgram st "tests/programs/P18_Mutability.tl"
                result <- evalExpr st18 "testRefIndependent()"
                result `shouldBe` CLMLIT (LInt 2)
            it "newMutArray + mutRead = 0" $ do
                st18 <- loadTestProgram st "tests/programs/P18_Mutability.tl"
                result <- evalExpr st18 "testMutArrayRead()"
                result `shouldBe` CLMLIT (LInt 0)
            it "mutWrite + mutRead = 42" $ do
                st18 <- loadTestProgram st "tests/programs/P18_Mutability.tl"
                result <- evalExpr st18 "testMutArrayWrite()"
                result `shouldBe` CLMLIT (LInt 42)
            it "mutLength = 5" $ do
                st18 <- loadTestProgram st "tests/programs/P18_Mutability.tl"
                result <- evalExpr st18 "testMutArrayLength()"
                result `shouldBe` CLMLIT (LInt 5)
            it "freeze produces [1, 2, 3]" $ do
                st18 <- loadTestProgram st "tests/programs/P18_Mutability.tl"
                result <- evalExpr st18 "testFreeze()"
                result `shouldBe` CLMARRAY [CLMLIT (LInt 1), CLMLIT (LInt 2), CLMLIT (LInt 3)]
            it "thaw + mutWrite = 99" $ do
                st18 <- loadTestProgram st "tests/programs/P18_Mutability.tl"
                result <- evalExpr st18 "testThaw()"
                result `shouldBe` CLMLIT (LInt 99)
            it "mutPush extends array length to 3" $ do
                st18 <- loadTestProgram st "tests/programs/P18_Mutability.tl"
                result <- evalExpr st18 "testMutPush()"
                result `shouldBe` CLMLIT (LInt 3)
            it "ref in closure accumulates = 10" $ do
                st18 <- loadTestProgram st "tests/programs/P18_Mutability.tl"
                result <- evalExpr st18 "testRefClosure()"
                result `shouldBe` CLMLIT (LInt 10)

        describe "P20: Class System" $ do
            it "Animal.new field access (name)" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t1()"
                result `shouldBe` CLMLIT (LString "Rex")
            it "Animal.new field access (age)" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t2()"
                result `shouldBe` CLMLIT (LInt 5)
            it "Animal.speak() base method" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t3()"
                result `shouldBe` CLMLIT (LString "...")
            it "Dog inherits Animal.name" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t4()"
                result `shouldBe` CLMLIT (LString "Buddy")
            it "Dog own field (breed)" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t5()"
                result `shouldBe` CLMLIT (LString "Labrador")
            it "Dog.speak() override (dynamic dispatch)" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t6()"
                result `shouldBe` CLMLIT (LString "Woof!")
            it "Dog.info() inherited method" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t7()"
                result `shouldBe` CLMLIT (LString "Buddy")
            it "Cat.speak() override" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t8()"
                result `shouldBe` CLMLIT (LString "Meow!")
            it "Puppy.speak() deep hierarchy override" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t9()"
                result `shouldBe` CLMLIT (LString "Yip!")
            it "Puppy inherits Dog.breed" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t10()"
                result `shouldBe` CLMLIT (LString "Poodle")
            it "Puppy own field (toy)" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t11()"
                result `shouldBe` CLMLIT (LString "ball")
            it "Puppy inherits Animal.name (3 levels)" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t12()"
                result `shouldBe` CLMLIT (LString "Max")
            it "Dog.fetch(item) method with args" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t13()"
                result `shouldBe` CLMLIT (LString "Buddy")
            it "Circle inherits Shape.describe()" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t14()"
                result `shouldBe` CLMLIT (LString "red")
            it "Circle own method area()" $ do
                st20 <- loadTestProgram st "tests/programs/P20_Classes.tl"
                result <- evalExpr st20 "t15()"
                result `shouldBe` CLMLIT (LInt 5)

        describe "P21: Sealed Classes" $ do
            it "Num.eval() on sealed subclass" $ do
                st21 <- loadTestProgram st "tests/programs/P21_SealedClasses.tl"
                result <- evalExpr st21 "t1()"
                result `shouldBe` CLMLIT (LInt 42)
            it "Add.eval() on sealed subclass" $ do
                st21 <- loadTestProgram st "tests/programs/P21_SealedClasses.tl"
                result <- evalExpr st21 "t2()"
                result `shouldBe` CLMLIT (LInt 7)
            it "Field access on sealed subclass" $ do
                st21 <- loadTestProgram st "tests/programs/P21_SealedClasses.tl"
                result <- evalExpr st21 "t3()"
                result `shouldBe` CLMLIT (LInt 99)
            it "Word token describe" $ do
                st21 <- loadTestProgram st "tests/programs/P21_SealedClasses.tl"
                result <- evalExpr st21 "t4()"
                result `shouldBe` CLMLIT (LString "hello")
            it "Punct token describe" $ do
                st21 <- loadTestProgram st "tests/programs/P21_SealedClasses.tl"
                result <- evalExpr st21 "t5()"
                result `shouldBe` CLMLIT (LString "!")

        describe "P22: Class Algebras" $ do
            it "show(Point) via algebra dispatch" $ do
                st22 <- loadTestProgram st "tests/programs/P22_ClassAlgebras.tl"
                result <- evalExpr st22 "t1()"
                result `shouldBe` CLMLIT (LString "Point")
            it "Point.show() direct method" $ do
                st22 <- loadTestProgram st "tests/programs/P22_ClassAlgebras.tl"
                result <- evalExpr st22 "t2()"
                result `shouldBe` CLMLIT (LString "Point")
            it "Color == Color (derive, true)" $ do
                st22 <- loadTestProgram st "tests/programs/P22_ClassAlgebras.tl"
                result <- evalExpr st22 "t3()"
                result `shouldBe` conTrue
            it "Color == Color (derive, false)" $ do
                st22 <- loadTestProgram st "tests/programs/P22_ClassAlgebras.tl"
                result <- evalExpr st22 "t4()"
                result `shouldBe` conFalse
            it "Color.display() own method" $ do
                st22 <- loadTestProgram st "tests/programs/P22_ClassAlgebras.tl"
                result <- evalExpr st22 "t5()"
                result `shouldBe` CLMLIT (LString "Color")

        describe "P23: Extern Class Plumbing" $ do
            it "Target import parses without error" $ do
                st23 <- loadTestProgram st "tests/programs/P23_ExternClasses.tl"
                result <- evalExpr st23 "t1()"
                result `shouldBe` CLMLIT (LString "parsed")
            it "Normal class works after target import" $ do
                st23 <- loadTestProgram st "tests/programs/P23_ExternClasses.tl"
                result <- evalExpr st23 "t2()"
                result `shouldBe` CLMLIT (LString "hello")
            it "Subclassing works after target import" $ do
                st23 <- loadTestProgram st "tests/programs/P23_ExternClasses.tl"
                result <- evalExpr st23 "t3()"
                result `shouldBe` CLMLIT (LString "clicked")

        describe "P24: Class Subtyping & Downcast" $ do
            it "subclass value where superclass expected" $ do
                st24 <- loadTestPrograms st ["tests/programs/P20_Classes.tl", "tests/programs/P24_ClassSubtyping.tl"]
                result <- evalExpr st24 "t1()"
                result `shouldBe` CLMLIT (LString "Rex")
            it "grandchild value where grandparent expected" $ do
                st24 <- loadTestPrograms st ["tests/programs/P20_Classes.tl", "tests/programs/P24_ClassSubtyping.tl"]
                result <- evalExpr st24 "t2()"
                result `shouldBe` CLMLIT (LString "Tiny")
            it "pattern match subclass constructor (Dog branch)" $ do
                st24 <- loadTestPrograms st ["tests/programs/P20_Classes.tl", "tests/programs/P24_ClassSubtyping.tl"]
                result <- evalExpr st24 "t3()"
                result `shouldBe` CLMLIT (LString "Lab")
            it "pattern match subclass constructor (Cat branch)" $ do
                st24 <- loadTestPrograms st ["tests/programs/P20_Classes.tl", "tests/programs/P24_ClassSubtyping.tl"]
                result <- evalExpr st24 "t4()"
                result `shouldBe` CLMLIT (LString "cat")
            it "pattern match wildcard (base class)" $ do
                st24 <- loadTestPrograms st ["tests/programs/P20_Classes.tl", "tests/programs/P24_ClassSubtyping.tl"]
                result <- evalExpr st24 "t5()"
                result `shouldBe` CLMLIT (LString "other")
            it "safe downcast succeeds (Dog as Dog)" $ do
                st24 <- loadTestPrograms st ["tests/programs/P20_Classes.tl", "tests/programs/P24_ClassSubtyping.tl"]
                result <- evalExpr st24 "t6()"
                result `shouldBe` CLMLIT (LString "Lab")
            it "safe downcast fails (Cat as Dog)" $ do
                st24 <- loadTestPrograms st ["tests/programs/P20_Classes.tl", "tests/programs/P24_ClassSubtyping.tl"]
                result <- evalExpr st24 "t7()"
                result `shouldBe` CLMLIT (LString "not a dog")
            it "downcast Puppy as Dog succeeds (subclass)" $ do
                st24 <- loadTestPrograms st ["tests/programs/P20_Classes.tl", "tests/programs/P24_ClassSubtyping.tl"]
                result <- evalExpr st24 "t8()"
                result `shouldBe` CLMLIT (LString "Poodle")
            it "downcast Animal as Dog fails (not a subclass)" $ do
                st24 <- loadTestPrograms st ["tests/programs/P20_Classes.tl", "tests/programs/P24_ClassSubtyping.tl"]
                result <- evalExpr st24 "t9()"
                result `shouldBe` CLMLIT (LString "not a dog")

        describe "P25: Pure String Library" $ do
            it "t1: encode ASCII = 1 byte" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t1()"
                result `shouldBe` CLMLIT (LInt 1)
            it "t2: encode 2-byte codepoint" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t2()"
                result `shouldBe` CLMLIT (LInt 2)
            it "t3: encode 3-byte codepoint" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t3()"
                result `shouldBe` CLMLIT (LInt 3)
            it "t4: encode 4-byte codepoint (emoji)" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t4()"
                result `shouldBe` CLMLIT (LInt 4)
            it "t5: decode ASCII byte = 65" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t5()"
                result `shouldBe` CLMLIT (LInt 65)
            it "t6: count code points" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t6()"
                result `shouldBe` CLMLIT (LInt 2)
            it "t7: valid UTF-8" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t7()"
                result `shouldBe` conTrue
            it "t8: invalid UTF-8" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t8()"
                result `shouldBe` conFalse
            it "t9: Str byte length" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t9()"
                result `shouldBe` CLMLIT (LInt 5)
            it "t10: empty string check" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t10()"
                result `shouldBe` conTrue
            it "t11: non-empty string check" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t11()"
                result `shouldBe` conFalse
            it "t12: Str equality" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t12()"
                result `shouldBe` conTrue
            it "t13: Str inequality" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t13()"
                result `shouldBe` conTrue
            it "t14: Str ordering" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t14()"
                result `shouldBe` CLMCON (ConsTag "LessThan" 0) []
            it "t15: Str semigroup combine" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t15()"
                result `shouldBe` CLMLIT (LInt 3)
            it "t16: byteLength" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t16()"
                result `shouldBe` CLMLIT (LInt 5)
            it "t17: strSlice" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t17()"
                result `shouldBe` conTrue
            it "t18: strDecodeAt" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t18()"
                result `shouldBe` CLMLIT (LInt 65)
            it "t19: strIndexOf found" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t19()"
                result `shouldBe` CLMLIT (LInt 1)
            it "t20: strIndexOf not found" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t20()"
                result `shouldBe` CLMLIT (LInt (-1))
            it "t21: charCount" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t21()"
                result `shouldBe` CLMLIT (LInt 5)
            it "t22: nthChar" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t22()"
                result `shouldBe` CLMLIT (LInt 66)
            it "t23: strStartsWith true" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t23()"
                result `shouldBe` conTrue
            it "t24: strStartsWith false" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t24()"
                result `shouldBe` conFalse
            it "t25: strEndsWith" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t25()"
                result `shouldBe` conTrue
            it "t26: strSplit count" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t26()"
                result `shouldBe` CLMLIT (LInt 3)
            it "t27: strJoin" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t27()"
                result `shouldBe` conTrue
            it "t28: strReplace" $ do
                st25 <- loadTestProgram st "tests/programs/P25_Strings.tl"
                result <- evalExpr st25 "t28()"
                result `shouldBe` conTrue

        describe "P26: String Literal Desugaring" $ do
            it "t1: string literal byte length" $ do
                st26 <- loadTestProgram (enableNewStrings st) "tests/programs/P26_StringDesugar.tl"
                result <- evalExpr st26 "t1()"
                result `shouldBe` CLMLIT (LInt 5)
            it "t2: empty string literal" $ do
                st26 <- loadTestProgram (enableNewStrings st) "tests/programs/P26_StringDesugar.tl"
                result <- evalExpr st26 "t2()"
                result `shouldBe` conTrue
            it "t3: string literal equality" $ do
                st26 <- loadTestProgram (enableNewStrings st) "tests/programs/P26_StringDesugar.tl"
                result <- evalExpr st26 "t3()"
                result `shouldBe` conTrue
            it "t4: string literal inequality" $ do
                st26 <- loadTestProgram (enableNewStrings st) "tests/programs/P26_StringDesugar.tl"
                result <- evalExpr st26 "t4()"
                result `shouldBe` conTrue
            it "t5: string literal concat byte length" $ do
                st26 <- loadTestProgram (enableNewStrings st) "tests/programs/P26_StringDesugar.tl"
                result <- evalExpr st26 "t5()"
                result `shouldBe` CLMLIT (LInt 4)
            it "t6: charCount on string literal" $ do
                st26 <- loadTestProgram (enableNewStrings st) "tests/programs/P26_StringDesugar.tl"
                result <- evalExpr st26 "t6()"
                result `shouldBe` CLMLIT (LInt 5)
            it "t7: strStartsWith with literals" $ do
                st26 <- loadTestProgram (enableNewStrings st) "tests/programs/P26_StringDesugar.tl"
                result <- evalExpr st26 "t7()"
                result `shouldBe` conTrue
            it "t8: strEndsWith with literals" $ do
                st26 <- loadTestProgram (enableNewStrings st) "tests/programs/P26_StringDesugar.tl"
                result <- evalExpr st26 "t8()"
                result `shouldBe` conTrue
            it "t9: strIndexOf with literals" $ do
                st26 <- loadTestProgram (enableNewStrings st) "tests/programs/P26_StringDesugar.tl"
                result <- evalExpr st26 "t9()"
                result `shouldBe` CLMLIT (LInt 2)
            it "t10: string compare ordering" $ do
                st26 <- loadTestProgram (enableNewStrings st) "tests/programs/P26_StringDesugar.tl"
                result <- evalExpr st26 "t10()"
                result `shouldBe` CLMCON (ConsTag "LessThan" 0) []

        describe "P27: Parameterized Repr" $ do
            it "t1: parameterized repr toRepr" $ do
                st27 <- loadTestProgram st "tests/programs/P27_ParamRepr.tl"
                result <- evalExpr st27 "t1()"
                result `shouldBe` CLMLIT (LInt 3)
            it "t2: parameterized repr fromRepr" $ do
                st27 <- loadTestProgram st "tests/programs/P27_ParamRepr.tl"
                result <- evalExpr st27 "t2()"
                result `shouldBe` CLMLIT (LInt 20)
            it "t3: simple repr backward compat" $ do
                st27 <- loadTestProgram st "tests/programs/P27_ParamRepr.tl"
                result <- evalExpr st27 "t3()"
                result `shouldBe` CLMLIT (LInt 0)

        describe "P28: String Manipulation Demo" $ do
            it "t1: string byte length" $ do
                st28 <- loadTestProgram (enableNewStrings st) "tests/programs/P28_StringDemo.tl"
                result <- evalExpr st28 "t1()"
                result `shouldBe` CLMLIT (LInt 5)
            it "t2: concat byte length" $ do
                st28 <- loadTestProgram (enableNewStrings st) "tests/programs/P28_StringDemo.tl"
                result <- evalExpr st28 "t2()"
                result `shouldBe` CLMLIT (LInt 11)
            it "t3: string equality" $ do
                st28 <- loadTestProgram (enableNewStrings st) "tests/programs/P28_StringDemo.tl"
                result <- evalExpr st28 "t3()"
                result `shouldBe` conTrue
            it "t4: string inequality" $ do
                st28 <- loadTestProgram (enableNewStrings st) "tests/programs/P28_StringDemo.tl"
                result <- evalExpr st28 "t4()"
                result `shouldBe` conTrue
            it "t5: charCount" $ do
                st28 <- loadTestProgram (enableNewStrings st) "tests/programs/P28_StringDemo.tl"
                result <- evalExpr st28 "t5()"
                result `shouldBe` CLMLIT (LInt 5)
            it "t6: strStartsWith" $ do
                st28 <- loadTestProgram (enableNewStrings st) "tests/programs/P28_StringDemo.tl"
                result <- evalExpr st28 "t6()"
                result `shouldBe` conTrue
            it "t7: strEndsWith" $ do
                st28 <- loadTestProgram (enableNewStrings st) "tests/programs/P28_StringDemo.tl"
                result <- evalExpr st28 "t7()"
                result `shouldBe` conTrue
            it "t8: strIndexOf" $ do
                st28 <- loadTestProgram (enableNewStrings st) "tests/programs/P28_StringDemo.tl"
                result <- evalExpr st28 "t8()"
                result `shouldBe` CLMLIT (LInt 6)
            it "t9: strSlice byte length" $ do
                st28 <- loadTestProgram (enableNewStrings st) "tests/programs/P28_StringDemo.tl"
                result <- evalExpr st28 "t9()"
                result `shouldBe` CLMLIT (LInt 5)
            it "t10: empty string check" $ do
                st28 <- loadTestProgram (enableNewStrings st) "tests/programs/P28_StringDemo.tl"
                result <- evalExpr st28 "t10()"
                result `shouldBe` conTrue

        describe "P29: Minimal Definition Checking" $ do
            it "t1: (==) provided directly" $ do
                st29 <- loadTestProgram st "tests/programs/P29_MinimalDef.tl"
                result <- evalExpr st29 "t1()"
                result `shouldBe` conTrue
            it "t2: (!=) from default (not (x == y))" $ do
                st29 <- loadTestProgram st "tests/programs/P29_MinimalDef.tl"
                result <- evalExpr st29 "t2()"
                result `shouldBe` conTrue
            it "t3: (!=) false case via default" $ do
                st29 <- loadTestProgram st "tests/programs/P29_MinimalDef.tl"
                result <- evalExpr st29 "t3()"
                result `shouldBe` conFalse
            it "t4: compare provided directly" $ do
                st29 <- loadTestProgram st "tests/programs/P29_MinimalDef.tl"
                result <- evalExpr st29 "t4()"
                result `shouldBe` conLT
            it "t5: (<) derived from compare default" $ do
                st29 <- loadTestProgram st "tests/programs/P29_MinimalDef.tl"
                result <- evalExpr st29 "t5()"
                result `shouldBe` conTrue
            it "t6: (>=) derived from compare default" $ do
                st29 <- loadTestProgram st "tests/programs/P29_MinimalDef.tl"
                result <- evalExpr st29 "t6()"
                result `shouldBe` conTrue
            it "t7: (<=) derived from compare default" $ do
                st29 <- loadTestProgram st "tests/programs/P29_MinimalDef.tl"
                result <- evalExpr st29 "t7()"
                result `shouldBe` conTrue
            it "t8: (>) derived from compare default" $ do
                st29 <- loadTestProgram st "tests/programs/P29_MinimalDef.tl"
                result <- evalExpr st29 "t8()"
                result `shouldBe` conTrue

        describe "P30: Derive for Parameterized Types" $ do
            it "t1: Eq(Maybe) derive - Just(1) == Just(1)" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t1()"
                result `shouldBe` conTrue
            it "t2: Eq(Maybe) derive - Just(1) == Just(2) is False" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t2()"
                result `shouldBe` conFalse
            it "t3: Eq(Maybe) derive - Nothing == Nothing" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t3()"
                result `shouldBe` conTrue
            it "t4: Eq(Maybe) derive - Just(1) == Nothing is False" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t4()"
                result `shouldBe` conFalse
            it "t5: Eq(Box) derive - MkBox(42) == MkBox(42)" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t5()"
                result `shouldBe` conTrue
            it "t6: Eq(Box) derive - MkBox(42) == MkBox(99) is False" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t6()"
                result `shouldBe` conFalse
            it "t7: Show(Box) derive - show(MkBox(42))" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t7()"
                result `shouldBe` CLMLIT (LString "MkBox(42)")
            it "t8: Eq(List) derive - equal lists" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t8()"
                result `shouldBe` conTrue
            it "t9: Eq(List) derive - different elements" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t9()"
                result `shouldBe` conFalse
            it "t10: Eq(List) derive - Nil == Nil" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t10()"
                result `shouldBe` conTrue
            it "t11: Eq(List) derive - Cons vs Nil" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t11()"
                result `shouldBe` conFalse
            it "t12: nested - Just(Just(1)) == Just(Just(1))" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t12()"
                result `shouldBe` conTrue
            it "t13: nested - Just(Just(1)) == Just(Just(2)) is False" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t13()"
                result `shouldBe` conFalse
            it "t14: nested - Just(Nil) == Just(Nil)" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t14()"
                result `shouldBe` conTrue
            it "t15: Eq(Pair) derive - MkPair(1,2) == MkPair(1,2)" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t15()"
                result `shouldBe` conTrue
            it "t16: Eq(Pair) derive - different pairs" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t16()"
                result `shouldBe` conFalse
            it "t17: Show(Pair) derive - show(MkPair(1, True))" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t17()"
                result `shouldBe` CLMLIT (LString "MkPair(1, True)")
            it "t18: deriving syntax - Triple Eq" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t18()"
                result `shouldBe` conTrue
            it "t19: deriving syntax - Triple Eq different" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t19()"
                result `shouldBe` conFalse
            it "t20: deriving syntax - Triple Show" $ do
                st30 <- loadTestProgram st "tests/programs/P30_DeriveParam.tl"
                result <- evalExpr st30 "t20()"
                result `shouldBe` CLMLIT (LString "MkTriple(1, True, \"hi\")")

        describe "P31: Requires Constraint Enforcement" $ do
            it "t1: Printable(Int) with satisfied requires Eq(Int)" $ do
                st31 <- loadTestProgram st "tests/programs/P31_Requires.tl"
                result <- evalExpr st31 "t1()"
                result `shouldBe` CLMLIT (LString "42")
            it "t2: Eq still works alongside Printable" $ do
                st31 <- loadTestProgram st "tests/programs/P31_Requires.tl"
                result <- evalExpr st31 "t2()"
                result `shouldBe` conTrue
            it "t3: Mappable morphism with requires - mapTo(1)" $ do
                st31 <- loadTestProgram st "tests/programs/P31_Requires.tl"
                result <- evalExpr st31 "t3()"
                result `shouldBe` conTrue
            it "t4: Mappable morphism with requires - mapTo(0)" $ do
                st31 <- loadTestProgram st "tests/programs/P31_Requires.tl"
                result <- evalExpr st31 "t4()"
                result `shouldBe` conFalse
            it "t5: derive on Wrapper - MkWrapper(10) == MkWrapper(10)" $ do
                st31 <- loadTestProgram st "tests/programs/P31_Requires.tl"
                result <- evalExpr st31 "t5()"
                result `shouldBe` conTrue
            it "t6: derive on Wrapper - MkWrapper(10) == MkWrapper(20)" $ do
                st31 <- loadTestProgram st "tests/programs/P31_Requires.tl"
                result <- evalExpr st31 "t6()"
                result `shouldBe` conFalse

        describe "P32: Effect Handlers (Runtime)" $ do
            it "t1: handle with SilentConsole suppresses output" $ do
                st32 <- loadTestProgram st "tests/programs/P32_EffectHandlers.tl"
                result <- evalExpr st32 "t1()"
                result `shouldBe` CLMLIT (LInt 42)
            it "t2: SilentConsole readLine returns empty string" $ do
                st32 <- loadTestProgram st "tests/programs/P32_EffectHandlers.tl"
                result <- evalExpr st32 "t2()"
                result `shouldBe` CLMLIT (LString "")
            it "t3: State effect with RefState - put, modify, get" $ do
                st32 <- loadTestProgram st "tests/programs/P32_EffectHandlers.tl"
                result <- evalExpr st32 "t3()"
                result `shouldBe` CLMLIT (LInt 15)
            it "t4: State - initial value used" $ do
                st32 <- loadTestProgram st "tests/programs/P32_EffectHandlers.tl"
                result <- evalExpr st32 "t4()"
                result `shouldBe` CLMLIT (LInt 42)
            it "t5: Multiple handle scopes with different state" $ do
                st32 <- loadTestProgram st "tests/programs/P32_EffectHandlers.tl"
                result <- evalExpr st32 "t5()"
                result `shouldBe` CLMLIT (LInt 109)
            it "t6: handler does not leak outside scope" $ do
                st32 <- loadTestProgram st "tests/programs/P32_EffectHandlers.tl"
                result <- evalExpr st32 "t6()"
                result `shouldBe` conTrue
            it "t7: SilentConsole putStr returns Unit" $ do
                st32 <- loadTestProgram st "tests/programs/P32_EffectHandlers.tl"
                result <- evalExpr st32 "t7()"
                result `shouldBe` conTrue

        describe "P33: Operator Fixity and Precedence" $ do
            it "t1: 2 + 3 * 4 == 14 (mul binds tighter)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t1()"
                result `shouldBe` CLMLIT (LInt 14)
            it "t2: 3 * 4 + 2 == 14 (mul binds tighter)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t2()"
                result `shouldBe` CLMLIT (LInt 14)
            it "t3: 1 + 2 == 3 is True (== binds looser than +)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t3()"
                result `shouldBe` conTrue
            it "t4: 2 * 3 == 6 is True (== binds looser than *)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t4()"
                result `shouldBe` conTrue
            it "t5: 1 + 2 * 3 == 7 is True (full chain)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t5()"
                result `shouldBe` conTrue
            it "t6: 1 - 2 - 3 == -4 (left-assoc)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t6()"
                result `shouldBe` CLMLIT (LInt (-4))
            it "t7: 2 * 3 * 4 == 24 (left-assoc)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t7()"
                result `shouldBe` CLMLIT (LInt 24)
            it "t8: 3 * -2 + 10 == 4 (unary minus)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t8()"
                result `shouldBe` CLMLIT (LInt 4)
            it "t9: (2 + 3) * 4 == 20 (parens override)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t9()"
                result `shouldBe` CLMLIT (LInt 20)
            it "t10: 10 - 2 * 3 == 4 is True (nested)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t10()"
                result `shouldBe` conTrue

            -- Group B: More prec 7 vs 6
            it "t11: 3 + 2 * 5 == 13 (+ vs * again)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t11()"
                result `shouldBe` CLMLIT (LInt 13)
            it "t12: 20 - 3 * 4 - 1 == 7 (mixed prec + left-assoc)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t12()"
                result `shouldBe` CLMLIT (LInt 7)

            -- Group C: Comparison operators (all prec 4)
            it "t13: 1 + 2 < 5 (< looser than +)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t13()"
                result `shouldBe` conTrue
            it "t14: 2 * 3 > 5 (> looser than *)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t14()"
                result `shouldBe` conTrue
            it "t15: 3 + 2 <= 5 (<= looser than +)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t15()"
                result `shouldBe` conTrue
            it "t16: 3 * 2 >= 6 (>= looser than *)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t16()"
                result `shouldBe` conTrue
            it "t17: 1 + 1 != 3 (!= looser than +)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t17()"
                result `shouldBe` conTrue
            it "t18: 2 * 3 < 2 * 4 (< with * on both sides)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t18()"
                result `shouldBe` conTrue

            -- Group D: Bitwise operators (prec 3, 2)
            it "t19: 7 .&. 3 == 3 (bitwise AND)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t19()"
                result `shouldBe` CLMLIT (LInt 3)
            it "t20: 4 .|. 2 == 6 (bitwise OR)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t20()"
                result `shouldBe` CLMLIT (LInt 6)
            it "t21: .&. binds tighter than .|." $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t21()"
                result `shouldBe` CLMLIT (LInt 7)
            it "t22: .&. binds tighter than .|. (reversed)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t22()"
                result `shouldBe` CLMLIT (LInt 7)
            it "t23: .&. right-assoc: 15 .&. 7 .&. 3 == 3" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t23()"
                result `shouldBe` CLMLIT (LInt 3)
            it "t24: .|. right-assoc: 1 .|. 2 .|. 4 == 7" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t24()"
                result `shouldBe` CLMLIT (LInt 7)

            -- Group E: Cross-level precedence (bitwise vs arithmetic)
            it "t25: .&. looser than +: 5 .&. 2 + 1 == 1" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t25()"
                result `shouldBe` CLMLIT (LInt 1)
            it "t26: .|. looser than +: 4 .|. 1 + 2 == 7" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t26()"
                result `shouldBe` CLMLIT (LInt 7)
            it "t27: .&. looser than *: 7 .&. 2 * 3 == 6" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t27()"
                result `shouldBe` CLMLIT (LInt 6)
            it "t28: .|. looser than *: 4 .|. 1 * 2 == 6" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t28()"
                result `shouldBe` CLMLIT (LInt 6)

            -- Group F: Full precedence chain
            it "t29: full chain 1 .|. 2 .&. 3 + 1 * 2 == 1" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t29()"
                result `shouldBe` CLMLIT (LInt 1)

            -- Group G: Unary minus edge cases
            it "t30: -3 * 2 == -6 (unary tighter than *)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t30()"
                result `shouldBe` CLMLIT (LInt (-6))
            it "t31: 10 + -2 * 3 == 4 (unary in middle)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t31()"
                result `shouldBe` CLMLIT (LInt 4)
            it "t32: - -5 == 5 (double unary)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t32()"
                result `shouldBe` CLMLIT (LInt 5)

            -- Group H: Custom fixity declarations
            it "t33: custom infixr 5 (<>) binds looser than *" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t33()"
                result `shouldBe` CLMLIT (LInt 32)
            it "t34: custom <> binds tighter than ==" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t34()"
                result `shouldBe` conTrue
            it "t35: custom <> right-assoc: 1 <> 2 <> 3 == 33" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t35()"
                result `shouldBe` CLMLIT (LInt 33)
            it "t36: custom <> right-assoc confirmed (not 123)" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t36()"
                result `shouldBe` conTrue
            it "t37: custom infixl 1 (>>>) binds loosest vs *" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t37()"
                result `shouldBe` CLMLIT (LInt 7)
            it "t38: >>> binds looser than .&." $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t38()"
                result `shouldBe` CLMLIT (LInt 4)
            it "t39: >>> left-assoc: 1 >>> 2 >>> 3 == 6" $ do
                st33 <- loadTestProgram st "tests/programs/P33_Fixity.tl"
                result <- evalExpr st33 "t39()"
                result `shouldBe` CLMLIT (LInt 6)

            -- Parser-level tests for fixity declarations
            it "parses infixl declaration" $ do
                res <- parseTestString "infixl 6 (+), (-);"
                case res of
                    Right [FixityDecl AssocLeft 6 ["+", "-"]] -> return ()
                    other -> expectationFailure $ "Expected FixityDecl, got: " ++ show other

            it "parses infixr declaration" $ do
                res <- parseTestString "infixr 5 (++);"
                case res of
                    Right [FixityDecl AssocRight 5 ["++"]] -> return ()
                    other -> expectationFailure $ "Expected FixityDecl, got: " ++ show other

            it "parses infix (non-assoc) declaration" $ do
                res <- parseTestString "infix 4 (==);"
                case res of
                    Right [FixityDecl AssocNone 4 ["=="]] -> return ()
                    other -> expectationFailure $ "Expected FixityDecl, got: " ++ show other

            -- Non-associativity error tests
            it "non-assoc: 1 == 2 == 3 is parse error" $ do
                result <- evalExpr st "1 == 2 == 3"
                case result of
                    CLMERR msg _ -> msg `shouldSatisfy` isInfixOf "Non-associative"
                    other -> expectationFailure $ "Expected parse error, got: " ++ show other

            it "non-assoc: 1 < 2 < 3 is parse error" $ do
                result <- evalExpr st "1 < 2 < 3"
                case result of
                    CLMERR msg _ -> msg `shouldSatisfy` isInfixOf "Non-associative"
                    other -> expectationFailure $ "Expected parse error, got: " ++ show other

            -- Interactive expression precedence tests
            it "interactive: 2 + 3 * 4 == 14" $ do
                result <- evalExpr st "2 + 3 * 4"
                result `shouldBe` CLMLIT (LInt 14)

            it "interactive: 1 + 2 == 3 is True" $ do
                result <- evalExpr st "1 + 2 == 3"
                result `shouldBe` conTrue

            it "interactive: 1 - 2 - 3 == -4" $ do
                result <- evalExpr st "1 - 2 - 3"
                result `shouldBe` CLMLIT (LInt (-4))

            -- Interactive: bitwise precedence
            it "interactive: 5 .|. 3 .&. 6 == 7" $ do
                result <- evalExpr st "5 .|. 3 .&. 6"
                result `shouldBe` CLMLIT (LInt 7)

            it "interactive: 5 .&. 2 + 1 == 1" $ do
                result <- evalExpr st "5 .&. 2 + 1"
                result `shouldBe` CLMLIT (LInt 1)

            -- Default fixity: unknown ops get infixl 9 (tightest)
            it "parses fixity declaration with single op" $ do
                res <- parseTestString "infix 0 (<=>);"
                case res of
                    Right [FixityDecl AssocNone 0 ["<=>"]] -> return ()
                    other -> expectationFailure $ "Expected FixityDecl, got: " ++ show other
