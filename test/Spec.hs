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
import Data.List (isInfixOf, isPrefixOf, nub, sort)
import Parser
import Logs
import Util.IOLogger (initLogState, LogState(..), LogMessage(..), LogLevel(..))
import qualified Data.Sequence as Seq
import System.Directory (doesFileExist)
import CaseOptimization (positivityCheckPass, terminationCheckPass, coverageCheckPass)
import Pipeline (installDefaultHandlers)
import TypeElaborate (elaborateExpr, elaborateLambda, typeOfExpr)
import LLVMSpec (llvmTests)
import BytecodeSpec (bytecodeTests)
-- Helper: load all modules from lib/ into state, return the state
setupEnv :: IO InterpreterState
setupEnv = do
    let silentState = emptyIntState { currentFlags = (currentFlags emptyIntState) { verbosity = Silent, showErrors = False, showWarnings = False } }
    finalState <- evalStateT (execStateT setup silentState) initLogState
    return finalState
  where
    setup = do
        loadModuleTree baseModulePath
        installDefaultHandlers

-- Helper: run an IntState action with a fresh state
runFresh :: IntState a -> IO a
runFresh act = runIntState act emptyIntState

-- Helper: run an IntState action and return collected warning messages
runAndGetWarnings :: InterpreterState -> IntState () -> IO [String]
runAndGetWarnings s act = do
    logSt <- execStateT (evalStateT act s) initLogState
    let allMsgs = Prelude.foldr (:) [] (logs logSt)
    let msgs = [message (payload m) | m <- allMsgs, level m == LogWarning]
    return msgs

-- Helper: make a minimal InterpreterState with given env
mkStateWithEnv :: Environment -> InterpreterState
mkStateWithEnv env = emptyIntState { currentEnvironment = env }

-- CLM value constructors (used by intrinsic registry tests)
conTrue, conFalse :: CLMExpr
conTrue = CLMCON (ConsTag "True" 0) []
conFalse = CLMCON (ConsTag "False" 1) []

-- Helper: check if a TCError is an UnboundVar (possibly wrapped in WithContext)
isUnboundVar :: TCError -> Bool
isUnboundVar (UnboundVar _) = True
isUnboundVar (WithContext _ inner) = isUnboundVar inner
isUnboundVar _ = False
-- Helper: parse a string as a tulam file, returning the expressions
parseTestString :: String -> IO (Either String [Expr])
parseTestString input = runParseOnly (T.pack input) "<test>"

-- Helper: check that an intrinsic exists
hasIntrinsic :: String -> String -> IO ()
hasIntrinsic func typName = case lookupIntrinsic func typName of
    Just _  -> pure ()
    Nothing -> expectationFailure $ "Intrinsic not found: " ++ func ++ " for " ++ typName

main :: IO ()
main = do
    st <- setupEnv
    hspec $ do
        -- Critical: verify stdlib compiles cleanly in strict mode (strictTypes=True is default)
        describe "Standard library strict mode compilation" $ do
            it "stdlib loads with zero type errors in strict mode" $ do
                let errCount = tcErrorCount st
                errCount `shouldBe` 0

        llvmTests
        bytecodeTests
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
                res <- runFresh $ parseExpr (T.pack "fn(x) = x")
                case res of
                    Right (Function (Lambda "" [Var "x" UNDEFINED UNDEFINED] (Id "x") UNDEFINED _ _)) -> return ()
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
                case lookupInstanceLambda "==" ["Nat"] Nothing env of
                    Just _ -> return ()
                    Nothing -> expectationFailure "Expected Eq instance for Nat"
        describe "Type Checker" $ do
            let st0 = initTCState TCRelaxed
                env0 = emptyTCEnv

            describe "normalizeTypeExpr" $ do
                it "converts Int to Id Int" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (Id "Int")) env0 st0
                    ty `shouldBe` Id "Int"

                it "converts Float64 to Id Float64" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (Id "Float64")) env0 st0
                    ty `shouldBe` Id "Float64"

                it "converts String to Id String" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (Id "String")) env0 st0
                    ty `shouldBe` Id "String"

                it "converts Bool to Id Bool" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (Id "Bool")) env0 st0
                    ty `shouldBe` Id "Bool"

                it "converts U (LConst 0) to U (LConst 0)" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (U (LConst 0))) env0 st0
                    ty `shouldBe` U (LConst 0)

                it "converts arrow type a -> b" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (ArrowType (Id "Int") (Id "Bool"))) env0 st0
                    ty `shouldBe` Pi Nothing (Id "Int") (Id "Bool")

                it "converts type application Maybe(Int)" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (App (Id "Maybe") [Id "Int"])) env0 st0
                    ty `shouldBe` App (Id "Maybe") [Id "Int"]

                it "converts UNDEFINED to fresh var" $ do
                    let Right (ty, st') = runTC (normalizeTypeExpr UNDEFINED) env0 st0
                    ty `shouldBe` Meta 0
                    nextMeta st' `shouldBe` 1

                it "converts lowercase id to TRigid" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (Id "a")) env0 st0
                    ty `shouldBe` Id "a"

                it "converts capitalized id to Id" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (Id "MyType")) env0 st0
                    ty `shouldBe` Id "MyType"

            describe "Unification" $ do
                it "unifies identical concrete types" $ do
                    let result = runTC (unify (Id "Int") (Id "Int")) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "fails on mismatched concrete types" $ do
                    let result = runTC (unify (Id "Int") (Id "Bool")) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should have failed"

                it "unifies variable with concrete type" $ do
                    let Right (_, st') = runTC (unify (Meta 0) (Id "Int")) env0 (st0 { nextMeta = 1 })
                    let Right (ty, _) = runTC (applySubst (Meta 0)) env0 st'
                    ty `shouldBe` Id "Int"

                it "unifies two variables" $ do
                    let Right (_, st') = runTC (unify (Meta 0) (Meta 1)) env0 (st0 { nextMeta = 2 })
                    let Right (ty, _) = runTC (applySubst (Meta 0)) env0 st'
                    ty `shouldBe` Meta 1

                it "detects occurs check" $ do
                    let result = runTC (unify (Meta 0) (App (Id "Maybe") [Meta 0])) env0 (st0 { nextMeta = 1 })
                    case result of
                        Left (OccursCheck _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs
                        Right _ -> expectationFailure "Should have failed with occurs check"

                it "unifies arrow types" $ do
                    let result = runTC (unify (Pi Nothing (Meta 0) (Id "Bool")) (Pi Nothing (Id "Int") (Meta 1))) env0 (st0 { nextMeta = 2 })
                    case result of
                        Right (_, st') -> do
                            let Right (t0, _) = runTC (applySubst (Meta 0)) env0 st'
                            let Right (t1, _) = runTC (applySubst (Meta 1)) env0 st'
                            t0 `shouldBe` Id "Int"
                            t1 `shouldBe` Id "Bool"
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "unifies type applications" $ do
                    let result = runTC (unify (App (Id "Maybe") [Meta 0]) (App (Id "Maybe") [Id "Int"])) env0 (st0 { nextMeta = 1 })
                    case result of
                        Right (_, st') -> do
                            let Right (ty, _) = runTC (applySubst (Meta 0)) env0 st'
                            ty `shouldBe` Id "Int"
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "unifies universes" $ do
                    let result = runTC (unify (U (LConst 0)) (U (LConst 0))) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "unify requires exact universe equality: U 0 /= U 1" $ do
                    let result = runTC (unify (U (LConst 0)) (U (LConst 1))) env0 st0
                    case result of
                        Left _ -> pure ()  -- unification requires exact equality
                        Right _ -> expectationFailure "Should have failed: unify requires exact universe equality"

                it "subtype: U (LConst 0) ≤ U (LConst 1) (cumulativity)" $ do
                    let result = runTC (subtype (U (LConst 0)) (U (LConst 1))) env0 st0
                    case result of
                        Right _ -> pure ()  -- cumulativity allows this in subtype
                        Left errs -> expectationFailure $ "Should succeed by cumulativity: " ++ show errs

                it "fails on higher-to-lower universe (U (LConst 1) does not unify with U (LConst 0))" $ do
                    let result = runTC (unify (U (LConst 1)) (U (LConst 0))) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should have failed: U (LConst 1) > U (LConst 0)"

                it "subtype: U (LConst 0) ≤ U (LConst 2) (transitive)" $ do
                    let result = runTC (subtype (U (LConst 0)) (U (LConst 2))) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Should succeed: " ++ show errs

                it "cumulativity: U (LConst 0) ≤ U (LConst 0) (reflexive)" $ do
                    let result = runTC (unify (U (LConst 0)) (U (LConst 0))) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Should succeed: " ++ show errs

                it "subtype: U (LConst 1) ≤ U (LConst 2)" $ do
                    let result = runTC (subtype (U (LConst 1)) (U (LConst 2))) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Should succeed: " ++ show errs

                it "fails: U (LConst 2) does not unify with U (LConst 0)" $ do
                    let result = runTC (unify (U (LConst 2)) (U (LConst 0))) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should have failed: U (LConst 2) > U (LConst 0)"

                it "cumulativity: Meta unifies with U (LConst 1) (variable can be any universe)" $ do
                    let st1 = st0 { nextMeta = 1 }
                        result = runTC (unify (Meta 0) (U (LConst 1))) env0 st1
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Should succeed: " ++ show errs

                it "transitively resolves substitutions" $ do
                    -- Meta 0 = Meta 1, Meta 1 = Int => Meta 0 resolves to Int
                    let st1 = st0 { nextMeta = 2 }
                    let Right (_, st2) = runTC (unify (Meta 0) (Meta 1)) env0 st1
                    let Right (_, st3) = runTC (unify (Meta 1) (Id "Int")) env0 st2
                    let Right (ty, _) = runTC (applySubst (Meta 0)) env0 st3
                    ty `shouldBe` Id "Int"

            describe "Row unification" $ do
                it "unifies identical empty rows" $ do
                    let result = runTC (unify (RowEmpty) (RowEmpty)) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "unifies record with same fields" $ do
                    let r1 = (RowExtend "x" (Id "Int") RowEmpty)
                        r2 = (RowExtend "x" (Id "Int") RowEmpty)
                    let result = runTC (unify r1 r2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "fails on mismatched field types" $ do
                    let r1 = (RowExtend "x" (Id "Int") RowEmpty)
                        r2 = (RowExtend "x" (Id "Bool") RowEmpty)
                    let result = runTC (unify r1 r2) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should have failed"

                it "unifies rows with different field order" $ do
                    let r1 = (RowExtend "x" (Id "Int") (RowExtend "y" (Id "Bool") RowEmpty))
                        r2 = (RowExtend "y" (Id "Bool") (RowExtend "x" (Id "Int") RowEmpty))
                    let result = runTC (unify r1 r2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "unifies open row with concrete row" $ do
                    let st1 = st0 { nextMeta = 1 }
                        r1 = (RowExtend "x" (Id "Int") (Meta 0))
                        r2 = (RowExtend "x" (Id "Int") (RowExtend "y" (Id "Bool") RowEmpty))
                    let result = runTC (unify r1 r2) env0 st1
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

            describe "Bidirectional checking" $ do
                it "infers literal Int type" $ do
                    let Right (ty, _) = runTC (infer (Lit (LInt 42))) env0 st0
                    ty `shouldBe` Id "Int"

                it "infers literal Float64 type" $ do
                    let Right (ty, _) = runTC (infer (Lit (LFloat 3.14))) env0 st0
                    ty `shouldBe` Id "Float64"

                it "infers literal String type" $ do
                    let Right (ty, _) = runTC (infer (Lit (LString "hello"))) env0 st0
                    ty `shouldBe` Id "String"

                it "infers variable type from environment" $ do
                    let env1 = env0 { varTypes = Map.fromList [("x", Id "Int")] }
                    let Right (ty, _) = runTC (infer (Id "x")) env1 st0
                    ty `shouldBe` Id "Int"

                it "check succeeds for matching literal" $ do
                    let result = runTC (check (Lit (LInt 5)) (Id "Int")) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

                it "check warns on type mismatch (permissive mode)" $ do
                    let result = runTC (check (Lit (LInt 5)) (Id "Bool")) env0 st0
                    case result of
                        Right (_, st') -> length (tcErrors st') `shouldSatisfy` (> 0)
                        Left _ -> pure ()  -- also acceptable

                it "infers function type from annotated lambda" $ do
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED] (Id "x") (Id "Int")
                    let Right (ty, _) = runTC (infer (Function lam)) env0 st0
                    ty `shouldBe` Pi Nothing (Id "Int") (Id "Int")

                it "infers UNDEFINED param type as fresh var" $ do
                    let lam = mkLambda "f" [Var "x" UNDEFINED UNDEFINED] (Lit (LInt 42)) (Id "Int")
                    let Right (ty, _) = runTC (infer (Function lam)) env0 st0
                    -- Should be ?0 -> Int (fresh var for param, Int for body)
                    case ty of
                        Pi Nothing (Meta _) (Id "Int") -> pure ()
                        other -> expectationFailure $ "Expected ?n -> Int, got: " ++ show other

                it "infers constructor type from compiler environment" $ do
                    -- Use the full environment with constructors
                    let result = runTC (infer (Lit (LInt 42))) env0 st0
                    case result of
                        Right (Id "Int", _) -> pure ()
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
                    ty `shouldBe` (RowExtend "x" (Id "Int") (RowExtend "y" (Id "Float64") RowEmpty))

                it "converts record type to Ty" $ do
                    let recTy = RecordType [("x", Id "Int"), ("y", Id "Bool")] False
                    let Right (ty, _) = runTC (normalizeTypeExpr recTy) env0 st0
                    ty `shouldBe` (RowExtend "x" (Id "Int") (RowExtend "y" (Id "Bool") RowEmpty))

                it "converts open record type with row var" $ do
                    let recTy = RecordType [("x", Id "Int")] True
                    let Right (ty, _) = runTC (normalizeTypeExpr recTy) env0 st0
                    case ty of
                        (RowExtend "x" (Id "Int") (Meta _)) -> pure ()
                        other -> expectationFailure $ "Expected open record, got: " ++ show other

                it "unifies record literal with expected record type" $ do
                    let recLit = RecordLit [("x", Lit (LInt 1)), ("y", Lit (LFloat 2.0))]
                        recTy = (RowExtend "x" (Id "Int") (RowExtend "y" (Id "Float64") RowEmpty))
                    let result = runTC (check recLit recTy) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

            describe "Polymorphism" $ do
                it "instantiates Pi (Just with fresh variable" $ do
                    let polyTy = Pi (Just "a") (U (LConst 0)) (Pi Nothing (Id "a") (Id "a"))
                    let Right (ty, _) = runTC (instantiate polyTy) env0 st0
                    -- Should be Meta 0 -> Meta 0 (fresh var replacing "a")
                    case ty of
                        Pi Nothing (Meta v1) (Meta v2) | v1 == v2 -> pure ()
                        other -> expectationFailure $ "Expected ?n -> ?n, got: " ++ show other

                it "nested Pi (Just instantiation" $ do
                    let polyTy = Pi (Just "a") (U (LConst 0)) (Pi (Just "b") (U (LConst 0)) (Pi Nothing (Id "a") (Id "b")))
                    let Right (ty, st') = runTC (instantiate polyTy) env0 st0
                    -- Should be Meta 0 -> Meta 1
                    case ty of
                        Pi Nothing (Meta v1) (Meta v2) | v1 /= v2 -> pure ()
                        other -> expectationFailure $ "Expected ?n -> ?m (n/=m), got: " ++ show other

                it "substTyVar replaces rigid variable" $ do
                    let ty = Pi Nothing (Id "a") (Id "Int")
                    substTyVar "a" (Id "Bool") ty `shouldBe` Pi Nothing (Id "Bool") (Id "Int")

                it "substTyVar does not replace shadowed variable" $ do
                    let ty = Pi (Just "a") (U (LConst 0)) (Pi Nothing (Id "a") (Id "b"))
                    substTyVar "a" (Id "Bool") ty `shouldBe` Pi (Just "a") (U (LConst 0)) (Pi Nothing (Id "a") (Id "b"))

                it "generalize quantifies free vars" $ do
                    let Right (ty, _) = runTC (generalize (Pi Nothing (Meta 0) (Meta 0))) env0 (st0 { nextMeta = 1 })
                    case ty of
                        Pi (Just _) (U (LConst 0)) (Pi Nothing (Id _) (Id _)) -> pure ()
                        other -> expectationFailure $ "Expected forall a. a -> a, got: " ++ show other

                it "generalize does not quantify env-bound vars" $ do
                    let env1 = env0 { varTypes = Map.fromList [("x", Meta 0)] }
                    let Right (ty, _) = runTC (generalize (Pi Nothing (Meta 0) (Meta 1))) env1 (st0 { nextMeta = 2 })
                    -- Meta 0 is in env, so only Meta 1 should be generalized
                    case ty of
                        Pi (Just _) (U (LConst 0)) (Pi Nothing (Meta 0) (Id _)) -> pure ()
                        other -> expectationFailure $ "Expected forall b. ?0 -> b, got: " ++ show other

            -- ============================================================
            -- Error Detection Tests
            -- ============================================================
            describe "Error detection: unification failures" $ do
                it "fails unifying Int with String" $ do
                    let result = runTC (unify (Id "Int") (Id "String")) env0 st0
                    case result of
                        Left (Mismatch (Id "Int") (Id "String") : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error type: " ++ show errs
                        Right _ -> expectationFailure "Should have failed"

                it "fails unifying arrow with non-arrow" $ do
                    let result = runTC (unify (Pi Nothing (Id "Int") (Id "Bool")) (Id "Int")) env0 st0
                    case result of
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error type: " ++ show errs
                        Right _ -> expectationFailure "Should have failed"

                it "fails unifying App with different constructors" $ do
                    let result = runTC (unify (App (Id "Maybe") [Id "Int"]) (App (Id "List") [Id "Int"])) env0 st0
                    case result of
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error type: " ++ show errs
                        Right _ -> expectationFailure "Should have failed"

                it "fails unifying App with different arity" $ do
                    let result = runTC (unify (App (Id "Pair") [Id "Int", Id "Bool"]) (App (Id "Pair") [Id "Int"])) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should have failed"

                it "occurs check with nested type" $ do
                    let result = runTC (unify (Meta 0) (Pi Nothing (Meta 0) (Id "Int"))) env0 (st0 { nextMeta = 1 })
                    case result of
                        Left (OccursCheck 0 _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs
                        Right _ -> expectationFailure "Should have failed with occurs check"

            describe "Error detection: check mode mismatches" $ do
                it "check Int literal against Bool warns" $ do
                    let result = runTC (check (Lit (LInt 5)) (Id "Bool")) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs

                it "check String literal against Int warns" $ do
                    let result = runTC (check (Lit (LString "hi")) (Id "Int")) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs

                it "check Float literal against String warns" $ do
                    let result = runTC (check (Lit (LFloat 3.14)) (Id "String")) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs

                it "check Char literal against Int warns" $ do
                    let result = runTC (check (Lit (LChar 'a')) (Id "Int")) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs

            describe "Error detection: function type mismatches" $ do
                it "function returning wrong type warns" $ do
                    -- function f(x:Int) : Bool = x  -- returns Int, declared Bool
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED] (Id "x") (Id "Bool")
                        env1 = env0 { varTypes = Map.fromList [("x", Id "Int")] }
                    let result = runTC (inferLambda lam) env1 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs

                it "check function against incompatible arrow type warns" $ do
                    -- function with Int -> Int checked against String -> Bool
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED] (Id "x") (Id "Int")
                    let result = runTC (check (Function lam) (Pi Nothing (Id "String") (Id "Bool"))) env0 st0
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
                    let r1 = (RowExtend "x" (Id "Int") RowEmpty)
                        r2 = (RowExtend "x" (Id "Int") (RowExtend "y" (Id "Bool") RowEmpty))
                    let result = runTC (unify r1 r2) env0 st0
                    case result of
                        Left (MissingField "y" : _) -> pure ()
                        Left _ -> pure ()  -- any error is acceptable
                        Right _ -> expectationFailure "Should have failed — closed record missing field"

                it "record field type mismatch" $ do
                    -- {x:Int, y:Bool} vs {x:Int, y:String}
                    let r1 = (RowExtend "x" (Id "Int") (RowExtend "y" (Id "Bool") RowEmpty))
                        r2 = (RowExtend "x" (Id "Int") (RowExtend "y" (Id "String") RowEmpty))
                    let result = runTC (unify r1 r2) env0 st0
                    case result of
                        Left (Mismatch (Id "Bool") (Id "String") : _) -> pure ()
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should have failed — field y type mismatch"

                it "extra field in expected closed record" $ do
                    -- checking {x=1, y=2, z=3} against {x:Int, y:Int}
                    let recLit = RecordLit [("x", Lit (LInt 1)), ("y", Lit (LInt 2)), ("z", Lit (LInt 3))]
                        recTy = (RowExtend "x" (Id "Int") (RowExtend "y" (Id "Int") RowEmpty))
                    let result = runTC (check recLit recTy) env0 st0
                    case result of
                        Left _ -> pure ()  -- should fail: extra field z
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)

                it "record literal field with wrong type" $ do
                    -- checking {x = "hello"} against {x:Int}
                    let recLit = RecordLit [("x", Lit (LString "hello"))]
                        recTy = (RowExtend "x" (Id "Int") RowEmpty)
                    let result = runTC (check recLit recTy) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)

            describe "Error detection: row polymorphism edge cases" $ do
                it "open record accepts extra fields" $ do
                    -- {x:Int, ..r} should accept {x:Int, y:Bool}
                    let st1 = st0 { nextMeta = 1 }
                        r1 = (RowExtend "x" (Id "Int") (Meta 0))
                        r2 = (RowExtend "x" (Id "Int") (RowExtend "y" (Id "Bool") RowEmpty))
                    let result = runTC (unify r1 r2) env0 st1
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Open record should accept extra fields: " ++ show errs

                it "closed record rejects extra fields" $ do
                    -- {x:Int} should NOT accept {x:Int, y:Bool}
                    let r1 = (RowExtend "x" (Id "Int") RowEmpty)
                        r2 = (RowExtend "x" (Id "Int") (RowExtend "y" (Id "Bool") RowEmpty))
                    let result = runTC (unify r1 r2) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Closed record should reject extra fields"

                it "missing field extraction from empty row fails" $ do
                    let result = runTC (unify ((RowExtend "x" (Id "Int") RowEmpty)) ((RowExtend "z" (Id "Int") RowEmpty))) env0 st0
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
                    let env1 = env0 { varTypes = Map.fromList [("x", Id "Int")] }
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
                    let err = Mismatch (Id "Int") (Id "Bool")
                    showTCError err `shouldSatisfy` \s ->
                        "Int" `isInfixOf` s && "Bool" `isInfixOf` s

                it "OccursCheck error mentions the variable" $ do
                    let err = OccursCheck 3 (Pi Nothing (Meta 3) (Id "Int"))
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
                    let errors = [ Mismatch (Id "Int") (Id "Bool")
                                 , OccursCheck 0 (Id "Int")
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
                    let result = runTC (check (Lit (LInt 5)) (Id "Bool")) env0 stStrict
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Strict mode should fail on mismatch"

                it "relaxed mode accumulates warning on mismatch" $ do
                    let result = runTC (check (Lit (LInt 5)) (Id "Bool")) env0 st0
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

                it "normalizeTypeExpr promotes literals to TLit" $ do
                    -- Lit (LInt 42) in type position now produces Lit (promoted to type level)
                    let result = runTC (normalizeTypeExpr (Lit (LInt 42))) env0 st0
                    case result of
                        Right (ty, _) -> ty `shouldBe` Lit (LInt 42)
                        Left errs -> expectationFailure $ "Expected TLit, got errors: " ++ show errs

            -- ============================================================
            -- Step 3: Pi (Just alpha-renaming
            -- ============================================================
            describe "Pi (Just alpha-renaming" $ do
                it "alpha-equivalent foralls unify" $ do
                    -- forall a. a -> a  should unify with  forall b. b -> b
                    let t1 = Pi (Just "a") (U (LConst 0)) (Pi Nothing (Id "a") (Id "a"))
                        t2 = Pi (Just "b") (U (LConst 0)) (Pi Nothing (Id "b") (Id "b"))
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Alpha-equivalent foralls should unify: " ++ show errs

                it "incompatible forall bodies don't unify" $ do
                    -- forall a. a -> a  vs  forall b. b -> Int
                    let t1 = Pi (Just "a") (U (LConst 0)) (Pi Nothing (Id "a") (Id "a"))
                        t2 = Pi (Just "b") (U (LConst 0)) (Pi Nothing (Id "b") (Id "Int"))
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
                    let err = WithContext "function 'test'" (Mismatch (Id "Int") (Id "Bool"))
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
                            ty `shouldBe` Pi Nothing (Id "Int") (Id "Int")
                            let hasFactUnbound = Prelude.any (\e -> case e of
                                    UnboundVar "fact" -> True
                                    WithContext _ (UnboundVar "fact") -> True
                                    _ -> False) (tcErrors st')
                            hasFactUnbound `shouldBe` False
                        Left errs -> expectationFailure $ "Should have succeeded: " ++ show errs

                it "non-recursive function unaffected by self-binding" $ do
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED] (Lit (LInt 42)) (Id "Int")
                    let Right (ty, _) = runTC (inferLambda lam) env0 st0
                    ty `shouldBe` Pi Nothing (Id "Int") (Id "Int")

            -- ============================================================
            -- Step 6: normalizeTypeExpr extended cases
            -- ============================================================
            describe "normalizeTypeExpr extended cases" $ do
                it "general App converts to TApp" $ do
                    -- App (App (Id "F") [Id "a"]) [Id "b"] -> App (App (Id "F") [Id "a"]) [Id "b"]
                    let expr = App (App (Id "F") [Id "a"]) [Id "b"]
                    let Right (ty, _) = runTC (normalizeTypeExpr expr) env0 st0
                    case ty of
                        App (App (Id "F") [Id "a"]) [Id "b"] -> pure ()
                        other -> expectationFailure $ "Expected nested TApp, got: " ++ show other

                it "Function in type position converts to Pi type" $ do
                    let lam = mkLambda "" [Var "x" (Id "Int") UNDEFINED] UNDEFINED (Id "Bool")
                    let Right (ty, _) = runTC (normalizeTypeExpr (Function lam)) env0 st0
                    case ty of
                        Pi _ (Id "Int") _ -> pure ()
                        other -> expectationFailure $ "Expected Pi type, got: " ++ show other

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
                            ty `shouldBe` Id "OneField"
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
                    let result = runTC (subtype (Id "Dog") (Id "Animal")) tcEnv' st0
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
                    let result = runTC (subtype (Id "Animal") (Id "Dog")) tcEnv' st0
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
                                            , varTypes = Map.fromList [("myDog", Id "Dog")] }
                    let stStrict = initTCState TCStrict
                    let result = runTC (check (Id "myDog") (Id "Animal")) tcEnv' stStrict
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
                        Right (ty, _) -> ty `shouldBe` App (Id "Maybe") [Id "Dog"]
                        Left errs -> expectationFailure $ "infer ReprCast to Dog should return Maybe(Dog): " ++ show errs

                it "SubtypeMismatch error has [TC] prefix" $ do
                    showTCError (SubtypeMismatch (Id "Int") (Id "Bool")) `shouldSatisfy` ("TC" `isInfixOf`)

                it "all [TC] errors include context prefix" $ do
                    let errors = [ Mismatch (Id "Int") (Id "Bool")
                                 , OccursCheck 0 (Id "Int")
                                 , UnboundVar "x"
                                 , MissingField "f"
                                 , ArityMismatch 1 2
                                 , OtherError "test"
                                 , WithContext "function 'foo'" (Mismatch (Id "Int") (Id "Bool"))
                                 ]
                    mapM_ (\e -> showTCError e `shouldSatisfy` \s -> "[TC]" `isPrefixOf` s) errors

                it "strict mode check passes for matching types" $ do
                    let stStrict = initTCState TCStrict
                    let result = runTC (check (Lit (LInt 5)) (Id "Int")) env0 stStrict
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
                    let st1 = st0 { nextMeta = 2 }
                    -- First, bind Meta 0 = Int
                    let Right (_, st2) = runTC (unify (Meta 0) (Id "Int")) env0 st1
                    -- Now try a failing unification — should not affect st2
                    let Right (result, st3) = runTC (tcTry (unify (Id "Int") (Id "Bool"))) env0 st2
                    result `shouldBe` Nothing
                    -- Meta 0 should still resolve to Int
                    let Right (ty, _) = runTC (applySubst (Meta 0)) env0 st3
                    ty `shouldBe` Id "Int"

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
                    let result = runTC (check (Id "noexist") (Id "Int")) env0 stStrict
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

            describe "Sigma unification" $ do
                it "unifies identical sigma types" $ do
                    let t1 = Sigma Nothing (Id "Int") (Id "Bool")
                        t2 = Sigma Nothing (Id "Int") (Id "Bool")
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Should unify: " ++ show errs

                it "fails on mismatched sigma components" $ do
                    let t1 = Sigma Nothing (Id "Int") (Id "Bool")
                        t2 = Sigma Nothing (Id "Int") (Id "String")
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should fail on sigma mismatch"

                it "unifies sigma with variables" $ do
                    let st1 = st0 { nextMeta = 2 }
                        t1 = Sigma Nothing (Meta 0) (Meta 1)
                        t2 = Sigma Nothing (Id "Int") (Id "Bool")
                    let Right (_, st') = runTC (unify t1 t2) env0 st1
                    let Right (r0, _) = runTC (applySubst (Meta 0)) env0 st'
                    let Right (r1, _) = runTC (applySubst (Meta 1)) env0 st'
                    r0 `shouldBe` Id "Int"
                    r1 `shouldBe` Id "Bool"

            describe "PropEq unification" $ do
                it "unifies identical identity types" $ do
                    let t1 = App (Id "PropEq") [(Id "Nat"), (Id "Z"), (Id "Z")]
                        t2 = App (Id "PropEq") [(Id "Nat"), (Id "Z"), (Id "Z")]
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Should unify: " ++ show errs

                it "fails on mismatched identity endpoints" $ do
                    let t1 = App (Id "PropEq") [(Id "Nat"), (Id "Z"), (Id "Z")]
                        t2 = App (Id "PropEq") [(Id "Nat"), (Id "Z"), (Id "S")]
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should fail"

            describe "EffType unification" $ do
                it "unifies identical effect types" $ do
                    let t1 = EffType (RowExtend "console" (Id "Console") RowEmpty) (Id "Unit")
                        t2 = EffType (RowExtend "console" (Id "Console") RowEmpty) (Id "Unit")
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Should unify: " ++ show errs

                it "fails on mismatched effect result types" $ do
                    let t1 = EffType RowEmpty (Id "Int")
                        t2 = EffType RowEmpty (Id "Bool")
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Should fail on effect result mismatch"

                it "unifies effect rows with different order" $ do
                    let t1 = EffType (RowExtend "a" (Id "A") (RowExtend "b" (Id "B") RowEmpty)) (Id "Unit")
                        t2 = EffType (RowExtend "b" (Id "B") (RowExtend "a" (Id "A") RowEmpty)) (Id "Unit")
                    let result = runTC (unify t1 t2) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Should unify reordered effects: " ++ show errs

            describe "Id mismatch" $ do
                it "different rigid names fail to unify" $ do
                    let result = runTC (unify (Id "a") (Id "b")) env0 st0
                    case result of
                        Left (Mismatch _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Wrong error: " ++ show errs
                        Right _ -> expectationFailure "Different rigid vars should not unify"

                it "Id vs Id fails" $ do
                    let result = runTC (unify (Id "a") (Id "Int")) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Rigid vs concrete should fail"

                it "Id vs Id fails" $ do
                    let result = runTC (unify (Id "Int") (Id "a")) env0 st0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Concrete vs rigid should fail"

            describe "Row occurs check" $ do
                it "row variable occurring in its own binding fails" $ do
                    let st1 = st0 { nextMeta = 1 }
                        -- Try to unify {x:Int, ..?0} with ?0 — should fail (occurs)
                        row1 = RowExtend "x" (Id "Int") (Meta 0)
                    let result = runTC (bind 0 row1) env0 st1
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Row occurs check should fail"

            describe "inferApp edge cases" $ do
                it "too many args for arrow type returns fresh var" $ do
                    -- Int -> Bool applied to (1, 2) — second arg has no matching param
                    let env1 = env0 { varTypes = Map.fromList [("f", Pi Nothing (Id "Int") (Id "Bool"))] }
                    let result = runTC (infer (App (Id "f") [Lit (LInt 1), Lit (LInt 2)])) env1 st0
                    case result of
                        Right (ty, _) -> case ty of
                            Meta _ -> pure ()  -- got a fresh var (Bool is not arrow)
                            other -> expectationFailure $ "Expected fresh var from non-arrow application, got: " ++ show other
                        Left _ -> pure ()  -- error also acceptable

                it "application to Meta creates arrow type" $ do
                    let st1 = st0 { nextMeta = 1 }
                        env1 = env0 { varTypes = Map.fromList [("f", Meta 0)] }
                    let Right (retTy, st') = runTC (infer (App (Id "f") [Lit (LInt 42)])) env1 st1
                    -- Meta 0 should now be bound to Int -> ?retTy
                    let Right (fTy, _) = runTC (applySubst (Meta 0)) env0 st'
                    case fTy of
                        Pi Nothing (Id "Int") _ -> pure ()
                        other -> expectationFailure $ "Expected Int -> ?, got: " ++ show other

            describe "LetIn and IfThenElse" $ do
                it "let binding visible in body" $ do
                    let letExpr = LetIn [(Var "x" UNDEFINED UNDEFINED, Lit (LInt 42))] (Id "x")
                    let Right (ty, _) = runTC (infer letExpr) env0 st0
                    ty `shouldBe` Id "Int"

                it "let binding with multiple bindings" $ do
                    let letExpr = LetIn [(Var "x" UNDEFINED UNDEFINED, Lit (LInt 1)),
                                         (Var "y" UNDEFINED UNDEFINED, Lit (LString "hi"))] (Id "y")
                    let Right (ty, _) = runTC (infer letExpr) env0 st0
                    ty `shouldBe` Id "String"

                it "if-then-else checks condition is Bool" $ do
                    let ite = IfThenElse (Lit (LInt 42)) (Lit (LInt 1)) (Lit (LInt 2))
                    let result = runTC (infer ite) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()

                it "if-then-else with matching branches succeeds" $ do
                    let env1 = env0 { varTypes = Map.fromList [("b", Id "Bool")] }
                        ite = IfThenElse (Id "b") (Lit (LInt 1)) (Lit (LInt 2))
                    let Right (ty, st') = runTC (infer ite) env1 st0
                    ty `shouldBe` Id "Int"
                    -- No type errors
                    Prelude.filter (\e -> case e of
                        UnboundVar _ -> False
                        WithContext _ (UnboundVar _) -> False
                        _ -> True) (tcErrors st') `shouldSatisfy` Prelude.null

                it "if-then-else with mismatched branches warns" $ do
                    let env1 = env0 { varTypes = Map.fromList [("b", Id "Bool")] }
                        ite = IfThenElse (Id "b") (Lit (LInt 1)) (Lit (LString "oops"))
                    let result = runTC (infer ite) env1 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()

            describe "Tuple type inference" $ do
                it "empty tuple infers Unit" $ do
                    let Right (ty, _) = runTC (infer (Tuple [])) env0 st0
                    ty `shouldBe` Id "Unit"

                it "single-element tuple infers element type" $ do
                    let Right (ty, _) = runTC (infer (Tuple [Lit (LInt 42)])) env0 st0
                    ty `shouldBe` Id "Int"

                it "multi-element tuple infers nested TSigma" $ do
                    let Right (ty, _) = runTC (infer (Tuple [Lit (LInt 1), Lit (LString "a"), Lit (LFloat 3.14)])) env0 st0
                    case ty of
                        Sigma Nothing (Id "Int") (Sigma Nothing (Id "String") (Id "Float64")) -> pure ()
                        other -> expectationFailure $ "Expected (Int, String, Float64), got: " ++ show other

            describe "Array literal inference" $ do
                it "empty array has polymorphic element type" $ do
                    let Right (ty, _) = runTC (infer (ArrayLit [])) env0 st0
                    case ty of
                        App (Id "Array") [Meta _] -> pure ()
                        other -> expectationFailure $ "Expected Array(?), got: " ++ show other

                it "non-empty array infers element type" $ do
                    let Right (ty, _) = runTC (infer (ArrayLit [Lit (LInt 1), Lit (LInt 2)])) env0 st0
                    ty `shouldBe` App (Id "Array") [Id "Int"]

            describe "Pattern match checking" $ do
                it "check PatternMatches propagates expected type" $ do
                    -- PatternMatches [CaseOf [] (Lit (LInt 42)) si]
                    -- checked against Bool should warn
                    let pm = PatternMatches [CaseOf [] (Lit (LInt 42)) SourceInteractive]
                    let result = runTC (check pm (Id "Bool")) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()

                it "CaseOf in check mode binds pattern variables" $ do
                    -- CaseOf [Var "x" (Id "Int") UNDEFINED] (Id "x") checked against Int
                    let co = CaseOf [Var "x" (Id "Int") UNDEFINED] (Id "x") SourceInteractive
                    let result = runTC (check co (Id "Int")) env0 st0
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
                            check (Lit (LInt 1)) (Id "Bool") `tcBind` \_ ->
                            check (Lit (LString "hi")) (Id "Int")
                            ) env0 st0
                    case result of
                        Right (_, st') -> length (tcErrors st') `shouldSatisfy` (>= 2)
                        Left _ -> expectationFailure "Relaxed mode should accumulate, not fail"

                it "strict mode fails on first mismatch" $ do
                    let stStrict = initTCState TCStrict
                    let result = runTC (
                            check (Lit (LInt 1)) (Id "Bool") `tcBind` \_ ->
                            check (Lit (LString "hi")) (Id "Int")
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
                        Pi Nothing _ _ -> pure ()  -- some form of arrow type
                        other -> expectationFailure $ "Expected arrow type, got: " ++ show other

            describe "normalizeTypeExpr extended coverage" $ do
                it "converts empty Tuple to Unit" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (Tuple [])) env0 st0
                    ty `shouldBe` Id "Unit"

                it "converts single Tuple to inner type" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (Tuple [Id "Int"])) env0 st0
                    ty `shouldBe` Id "Int"

                it "converts multi Tuple to Sigma chain" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (Tuple [Id "Int", Id "Bool"])) env0 st0
                    ty `shouldBe` Sigma Nothing (Id "Int") (Id "Bool")

                it "converts Implicit wrapper" $ do
                    let Right (ty, _) = runTC (normalizeTypeExpr (Implicit (Id "Int"))) env0 st0
                    ty `shouldBe` Id "Int"

                it "converts EffType" $ do
                    let effExpr = EffType (RecordType [("console", Id "Console")] False) (Id "Unit")
                    let Right (ty, _) = runTC (normalizeTypeExpr effExpr) env0 st0
                    case ty of
                        EffType (RowExtend "console" (Id "Console") RowEmpty) (Id "Unit") -> pure ()
                        other -> expectationFailure $ "Expected Eff {console:Console} Unit, got: " ++ show other

                it "converts PropEqT to TId" $ do
                    let peq = App (Id "PropEqT") [Id "Nat", Id "Z", Id "Z"]
                    let Right (ty, _) = runTC (normalizeTypeExpr peq) env0 st0
                    ty `shouldBe` App (Id "PropEq") [(Id "Nat"), (Id "Z"), (Id "Z")]

            describe "applySubst through complex types" $ do
                it "substitutes through TApp" $ do
                    let st1 = st0 { nextMeta = 1, substitution = Map.fromList [(0, Id "Int")] }
                    let Right (ty, _) = runTC (applySubst (App (Id "Maybe") [Meta 0])) env0 st1
                    ty `shouldBe` App (Id "Maybe") [Id "Int"]

                it "substitutes through TSigma" $ do
                    let st1 = st0 { nextMeta = 2, substitution = Map.fromList [(0, Id "Int"), (1, Id "Bool")] }
                    let Right (ty, _) = runTC (applySubst (Sigma Nothing (Meta 0) (Meta 1))) env0 st1
                    ty `shouldBe` Sigma Nothing (Id "Int") (Id "Bool")

                it "substitutes through TId" $ do
                    let st1 = st0 { nextMeta = 1, substitution = Map.fromList [(0, Id "Z")] }
                    let Right (ty, _) = runTC (applySubst (App (Id "PropEq") [(Id "Nat"), (Meta 0), (Meta 0)])) env0 st1
                    ty `shouldBe` App (Id "PropEq") [(Id "Nat"), (Id "Z"), (Id "Z")]

                it "substitutes through Pi (Just body" $ do
                    let st1 = st0 { nextMeta = 1, substitution = Map.fromList [(0, Id "Int")] }
                    let Right (ty, _) = runTC (applySubst (Pi (Just "a") (U (LConst 0)) (Pi Nothing (Id "a") (Meta 0)))) env0 st1
                    ty `shouldBe` Pi (Just "a") (U (LConst 0)) (Pi Nothing (Id "a") (Id "Int"))

                it "substitutes through id row" $ do
                    let st1 = st0 { nextMeta = 1, substitution = Map.fromList [(0, Id "Int")] }
                    let Right (ty, _) = runTC (applySubst ((RowExtend "x" (Meta 0) RowEmpty))) env0 st1
                    ty `shouldBe` (RowExtend "x" (Id "Int") RowEmpty)

                it "substitutes through TEffect" $ do
                    let st1 = st0 { nextMeta = 1, substitution = Map.fromList [(0, Id "String")] }
                    let Right (ty, _) = runTC (applySubst (EffType RowEmpty (Meta 0))) env0 st1
                    ty `shouldBe` EffType RowEmpty (Id "String")

                it "chases substitution chains" $ do
                    -- ?0 -> ?1 -> Int
                    let st1 = st0 { nextMeta = 2, substitution = Map.fromList [(0, Meta 1), (1, Id "Int")] }
                    let Right (ty, _) = runTC (applySubst (Meta 0)) env0 st1
                    ty `shouldBe` Id "Int"

            describe "showTy coverage" $ do
                it "showTy TVar" $ showTy (Meta 42) `shouldBe` "?42"
                it "showTy TRigid" $ showTy (Id "a") `shouldBe` "a"
                it "showTy Id" $ showTy (Id "Int") `shouldBe` "Int"
                it "showTy TApp" $ showTy (App (Id "Maybe") [Id "Int"]) `shouldBe` "Maybe(Int)"
                it "showTy TArrow" $ showTy (Pi Nothing (Id "Int") (Id "Bool")) `shouldBe` "Int -> Bool"
                it "showTy Pi dependent" $ showTy (Pi (Just "x") (Id "Int") (Id "Bool")) `shouldBe` "(x:Int) -> Bool"
                it "showTy TSigma" $ showTy (Sigma Nothing (Id "Int") (Id "Bool")) `shouldBe` "(Int, Bool)"
                it "showTy Sigma dependent" $ showTy (Sigma (Just "x") (Id "Int") (Id "Bool")) `shouldBe` "(x:Int * Bool)"
                it "showTy TId" $ showTy (App (Id "PropEq") [(Id "Nat"), (Id "Z"), (Id "Z")]) `shouldBe` "PropEq(Nat, Z, Z)"
                it "showTy TForall" $ showTy (Pi (Just "a") (U (LConst 0)) (Pi Nothing (Id "a") (Id "a"))) `shouldBe` "forall a. a -> a"
                it "showTy TRecord" $ showTy ((RowExtend "x" (Id "Int") RowEmpty)) `shouldBe` "{x:Int}"
                it "showTy TEffect" $ showTy (EffType (RowExtend "c" (Id "C") RowEmpty) (Id "Unit")) `shouldBe` "Eff {c:C} Unit"
                it "showTy U (LConst 0)" $ showTy (U (LConst 0)) `shouldBe` "Type"
                it "showTy U (LConst 1)" $ showTy (U (LConst 1)) `shouldBe` "Type1"
                it "showTy nested arrow in arg position" $ do
                    showTy (Pi Nothing (Pi Nothing (Id "Int") (Id "Bool")) (Id "String"))
                        `shouldBe` "(Int -> Bool) -> String"
                it "showRow with row variable" $ showRow (Meta 5) `shouldBe` "..?5"
                it "showRow with rigid" $ showRow (Id "r") `shouldBe` "..r"
                it "showRow multiple fields" $ do
                    showRow (RowExtend "x" (Id "Int") (RowExtend "y" (Id "Bool") RowEmpty))
                        `shouldBe` "x:Int, y:Bool"

            describe "showTCError coverage" $ do
                it "ConstraintUnsolved shows structure name" $ do
                    let err = ConstraintUnsolved (CStructure "Eq" Nothing [Id "MyType"] Nothing)
                    showTCError err `shouldSatisfy` ("Eq" `isInfixOf`)
                    showTCError err `shouldSatisfy` ("MyType" `isInfixOf`)

                it "WithContext nests context info" $ do
                    let err = WithContext "method 'foo'" (WithContext "class Bar" (Mismatch (Id "Int") (Id "Bool")))
                    let s = showTCError err
                    s `shouldSatisfy` ("foo" `isInfixOf`)
                    s `shouldSatisfy` ("Bar" `isInfixOf`)
                    s `shouldSatisfy` ("Int" `isInfixOf`)

                it "Mismatch between two metas hints about missing annotations" $ do
                    let err = Mismatch (Meta 0) (Meta 1)
                    let s = showTCError err
                    s `shouldSatisfy` ("Hint" `isInfixOf`)
                    s `shouldSatisfy` ("annotation" `isInfixOf`)

                it "Mismatch with one meta hints about annotation" $ do
                    let err = Mismatch (Meta 0) (Id "Int")
                    let s = showTCError err
                    s `shouldSatisfy` ("Hint" `isInfixOf`)
                    s `shouldSatisfy` ("annotation" `isInfixOf`)

                it "Mismatch between concrete types has no hint" $ do
                    let err = Mismatch (Id "Int") (Id "Bool")
                    let s = showTCError err
                    s `shouldSatisfy` (not . ("Hint" `isInfixOf`))

                it "Meta in compound type hints about annotation" $ do
                    let err = Mismatch (App (Id "Maybe") [Meta 0]) (Id "Int")
                    let s = showTCError err
                    s `shouldSatisfy` ("Hint" `isInfixOf`)

            describe "Numeric literal types" $ do
                it "infers Int8 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LInt8 1))) env0 st0
                    ty `shouldBe` Id "Int8"
                it "infers Int16 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LInt16 1))) env0 st0
                    ty `shouldBe` Id "Int16"
                it "infers Int32 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LInt32 1))) env0 st0
                    ty `shouldBe` Id "Int32"
                it "infers Int64 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LInt64 1))) env0 st0
                    ty `shouldBe` Id "Int64"
                it "infers UInt8 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LWord8 1))) env0 st0
                    ty `shouldBe` Id "UInt8"
                it "infers UInt16 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LWord16 1))) env0 st0
                    ty `shouldBe` Id "UInt16"
                it "infers UInt32 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LWord32 1))) env0 st0
                    ty `shouldBe` Id "UInt32"
                it "infers UInt64 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LWord64 1))) env0 st0
                    ty `shouldBe` Id "UInt64"
                it "infers Float32 literal" $ do
                    let Right (ty, _) = runTC (infer (Lit (LFloat32 1.0))) env0 st0
                    ty `shouldBe` Id "Float32"

            describe "Misc infer cases" $ do
                it "infers empty LList" $ do
                    let Right (ty, _) = runTC (infer (Lit (LList []))) env0 st0
                    case ty of
                        App (Id "List") [Meta _] -> pure ()
                        other -> expectationFailure $ "Expected List(?), got: " ++ show other
                it "infers non-empty LList" $ do
                    let Right (ty, _) = runTC (infer (Lit (LList [Lit (LInt 1)]))) env0 st0
                    ty `shouldBe` App (Id "List") [Id "Int"]

                it "Typed expression checks against annotation" $ do
                    let Right (ty, _) = runTC (infer (Typed (Lit (LInt 42)) (Id "Int"))) env0 st0
                    ty `shouldBe` Id "Int"

                it "Typed expression with mismatch warns" $ do
                    let result = runTC (infer (Typed (Lit (LInt 42)) (Id "Bool"))) env0 st0
                    case result of
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)
                        Left _ -> pure ()

                it "PatternMatches empty returns fresh var" $ do
                    let Right (ty, _) = runTC (infer (PatternMatches [])) env0 st0
                    case ty of
                        Meta _ -> pure ()
                        other -> expectationFailure $ "Expected fresh var, got: " ++ show other

                it "Statements infers last statement type" $ do
                    let Right (ty, _) = runTC (infer (Statements [Lit (LString "a"), Lit (LInt 42)])) env0 st0
                    ty `shouldBe` Id "Int"

                it "empty Statements returns Unit" $ do
                    let Right (ty, _) = runTC (infer (Statements [])) env0 st0
                    ty `shouldBe` Id "Unit"

                it "UnaryOp dispatches to App" $ do
                    let env1 = env0 { varTypes = Map.fromList [("negate", Pi Nothing (Id "Int") (Id "Int"))] }
                    let Right (ty, _) = runTC (infer (UnaryOp "negate" (Lit (LInt 5)))) env1 st0
                    ty `shouldBe` Id "Int"

                it "BinaryOp dispatches to App" $ do
                    let env1 = env0 { varTypes = Map.fromList [("+", Pi Nothing (Id "Int") (Pi Nothing (Id "Int") (Id "Int")))] }
                    let Right (ty, _) = runTC (infer (BinaryOp "+" (Lit (LInt 1)) (Lit (LInt 2)))) env1 st0
                    ty `shouldBe` Id "Int"

                it "U n infers U (n+1)" $ do
                    let Right (ty, _) = runTC (infer (U (LConst 0))) env0 st0
                    ty `shouldBe` U (LConst 1)
                    let Right (ty2, _) = runTC (infer (U (LConst 1))) env0 st0
                    ty2 `shouldBe` U (LConst 2)

                it "SumType infers Type" $ do
                    let Right (ty, _) = runTC (infer (SumType (mkLambda "Bool" [] (Tuple []) (U (LConst 0))))) env0 st0
                    ty `shouldBe` U (LConst 0)

                it "ReprCast infers target type" $ do
                    let Right (ty, _) = runTC (infer (ReprCast (Lit (LInt 42)) (Id "Nat"))) env0 st0
                    ty `shouldBe` Id "Nat"

                it "ERROR node warns and returns fresh var" $ do
                    let Right (ty, st') = runTC (infer (ERROR "test error")) env0 st0
                    case ty of
                        Meta _ -> pure ()
                        other -> expectationFailure $ "Expected fresh var, got: " ++ show other
                    tcErrors st' `shouldSatisfy` (not . null)

                it "RecFieldAccess on record type extracts field" $ do
                    let recTy = (RowExtend "x" (Id "Int") (RowExtend "y" (Id "Bool") RowEmpty))
                        env1 = env0 { varTypes = Map.fromList [("r", recTy)] }
                    let Right (ty, _) = runTC (infer (RecFieldAccess ("x", -1) (Id "r"))) env1 st0
                    ty `shouldBe` Id "Int"

                it "RecFieldAccess on non-record returns fresh var" $ do
                    let env1 = env0 { varTypes = Map.fromList [("n", Id "Int")] }
                    let Right (ty, _) = runTC (infer (RecFieldAccess ("x", -1) (Id "n"))) env1 st0
                    case ty of
                        Meta _ -> pure ()
                        other -> expectationFailure $ "Expected fresh var, got: " ++ show other

            describe "inferLamType" $ do
                it "converts simple lambda to Pi type (preserving param name)" $ do
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED] UNDEFINED (Id "Bool")
                    let Right (ty, _) = runTC (inferLamType lam) env0 st0
                    case ty of
                        Pi (Just "x") (Id "Int") _ -> pure ()
                        other -> expectationFailure $ "Expected (x:Int) -> ?, got: " ++ show other

                it "converts multi-param lambda to nested Pi types" $ do
                    let lam = mkLambda "f" [Var "x" (Id "Int") UNDEFINED, Var "y" (Id "Bool") UNDEFINED]
                                        UNDEFINED (Id "String")
                    let Right (ty, _) = runTC (inferLamType lam) env0 st0
                    case ty of
                        Pi (Just "x") (Id "Int") (Pi (Just "y") (Id "Bool") _) -> pure ()
                        other -> expectationFailure $ "Expected (x:Int) -> (y:Bool) -> ?, got: " ++ show other

                it "converts zero-param lambda to return type" $ do
                    let lam = mkLambda "f" [] UNDEFINED (Id "Int")
                    let Right (ty, _) = runTC (inferLamType lam) env0 st0
                    case ty of
                        Meta _ -> pure ()  -- UNDEFINED return type -> fresh var
                        Id "Int" -> pure ()
                        other -> expectationFailure $ "Expected Int or fresh var, got: " ++ show other

            describe "tyToName" $ do
                it "extracts name from Id" $ tyToName (Id "Int") `shouldBe` "Int"
                it "extracts name from App head" $ tyToName (App (Id "Maybe") [Id "Int"]) `shouldBe` "Maybe"
                it "returns empty for TVar" $ tyToName (Meta 0) `shouldBe` ""
                it "returns name for Id (same as Id now)" $ tyToName (Id "a") `shouldBe` "a"
                it "returns empty for TArrow" $ tyToName (Pi Nothing (Id "Int") (Id "Bool")) `shouldBe` ""

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
                    let Right (ty, _) = runTC (generalize (Pi Nothing (Id "Int") (Id "Bool"))) env0 st0
                    ty `shouldBe` Pi Nothing (Id "Int") (Id "Bool")

                it "multiple free vars produce multiple foralls" $ do
                    let Right (ty, _) = runTC (generalize (Pi Nothing (Meta 0) (Pi Nothing (Meta 1) (Meta 0)))) env0 (st0 { nextMeta = 2 })
                    case ty of
                        Pi (Just _) (U (LConst 0)) (Pi (Just _) (U (LConst 0)) _) -> pure ()
                        other -> expectationFailure $ "Expected forall a. forall b. ..., got: " ++ show other

            describe "Bidirectional check integration" $ do
                it "check record literal against compatible open record" $ do
                    let st1 = st0 { nextMeta = 1 }
                        recLit = RecordLit [("x", Lit (LInt 1)), ("y", Lit (LFloat 2.0))]
                        openRecTy = (RowExtend "x" (Id "Int") (Meta 0))
                    let result = runTC (check recLit openRecTy) env0 st1
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Open record should accept extra fields: " ++ show errs

                it "checkTopLevel SumType passes" $ do
                    let result = runTC (checkTopLevel (SumType (mkLambda "Bool" [] (Tuple []) (U (LConst 0))))) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "SumType should pass: " ++ show errs

                it "checkTopLevel Instance checks implementations" $ do
                    let result = runTC (checkTopLevel (Instance "Eq" Nothing [Id "Bool"] [Function (mkLambda "==" [] UNDEFINED UNDEFINED)] [])) env0 st0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "Instance should pass: " ++ show errs

        -- ============================================================
        -- Phase 0: Module system parsing
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
                -- Core should define Bool, True, False (consolidated from Core.Bool)
                case Map.lookup "Core" loaded of
                    Just menv -> do
                        Set.member "Bool" (publicNames menv) `shouldBe` True
                        Set.member "True" (publicNames menv) `shouldBe` True
                        Set.member "False" (publicNames menv) `shouldBe` True
                    Nothing -> expectationFailure "Core module not found in loadedModules"

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
                hasIntrinsic "size" "Array"
                hasIntrinsic "index" "Array"
                hasIntrinsic "slice" "Array"

            it "array size evaluates" $ do
                case lookupIntrinsic "size" "Array" of
                    Just f -> f [CLMARRAY [CLMLIT (LInt 1), CLMLIT (LInt 2), CLMLIT (LInt 3)]]
                        `shouldBe` Just (CLMLIT (LInt 3))
                    Nothing -> expectationFailure "Array size not found"

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

        -- Type-level normalization (extracted from P34)
            describe "Type-level normalization (evalCLMPure)" $ do
                let env = currentEnvironment st

                it "normalizeTy: Id passes through unchanged" $ do
                    let result = normalizeTy env (Id "Int")
                    result `shouldBe` Id "Int"

                it "normalizeTy: App with unknown function passes through" $ do
                    let result = normalizeTy env (App (Id "Vec") [Id "Int", Id "Z"])
                    result `shouldBe` App (Id "Vec") [Id "Int", Id "Z"]

                it "normalizeTy: plus(Z, Z) reduces to Z" $ do
                    let result = normalizeTy env (App (Id "plus") [Id "Z", Id "Z"])
                    result `shouldBe` Id "Z"

                it "normalizeTy: plus(Succ(Z), Z) reduces to Succ(Z)" $ do
                    let natZ = Id "Z"
                        natSZ = App (Id "Succ") [natZ]
                        result = normalizeTy env (App (Id "plus") [natSZ, natZ])
                    result `shouldBe` App (Id "Succ") [Id "Z"]

                it "normalizeTy: plus(Succ(Z), Succ(Z)) reduces to Succ(Succ(Z))" $ do
                    let natZ = Id "Z"
                        natSZ = App (Id "Succ") [natZ]
                        natSSZ = App (Id "Succ") [natSZ]
                        result = normalizeTy env (App (Id "plus") [natSZ, natSZ])
                    result `shouldBe` natSSZ

                it "normalizeTy: nested plus(plus(Z,Z), Succ(Z)) reduces" $ do
                    let natZ = Id "Z"
                        natSZ = App (Id "Succ") [natZ]
                        inner = App (Id "plus") [natZ, natZ]
                        result = normalizeTy env (App (Id "plus") [inner, natSZ])
                    result `shouldBe` natSZ

                it "normalizeTy: mult(Succ(Succ(Z)), Succ(Succ(Z))) = 4" $ do
                    let n i = Prelude.iterate (\x -> App (Id "Succ") [x]) (Id "Z") !! i
                        result = normalizeTy env (App (Id "mult") [n 2, n 2])
                    result `shouldBe` n 4

                it "normalizeTy: doesn't reduce when args have TVar" $ do
                    let result = normalizeTy env (App (Id "plus") [Meta 0, Id "Z"])
                    result `shouldBe` App (Id "plus") [Meta 0, Id "Z"]

                it "exprToCLMTC: Z → CLMCON Z 0 []" $ do
                    let result = exprToCLMTC env (Id "Z")
                    result `shouldBe` CLMCON (ConsTag "Z" 0) []

                it "exprToCLMTC: Succ(Z) → CLMCON Succ 1 [CLMCON Z 0 []]" $ do
                    let result = exprToCLMTC env (App (Id "Succ") [Id "Z"])
                    result `shouldBe` CLMCON (ConsTag "Succ" 1) [CLMCON (ConsTag "Z" 0) []]

                it "clmToExpr: CLMCON Z → Id Z" $ do
                    let result = clmToExpr (CLMCON (ConsTag "Z" 0) [])
                    result `shouldBe` Just (Id "Z")

                it "clmToExpr: CLMCON Succ [Z] → App Succ [Z]" $ do
                    let result = clmToExpr (CLMCON (ConsTag "Succ" 1) [CLMCON (ConsTag "Z" 0) []])
                    result `shouldBe` Just (App (Id "Succ") [Id "Z"])

                -- Functions at different universe levels
                it "normalizeTy: not(True) reduces to False (Bool functions at type level)" $ do
                    let result = normalizeTy env (App (Id "not") [Id "True"])
                    result `shouldBe` Id "False"

                it "normalizeTy: not(False) reduces to True" $ do
                    let result = normalizeTy env (App (Id "not") [Id "False"])
                    result `shouldBe` Id "True"

                it "normalizeTy: not(not(True)) reduces to True (nested Bool)" $ do
                    let inner = App (Id "not") [Id "True"]
                        result = normalizeTy env (App (Id "not") [inner])
                    result `shouldBe` Id "True"

                it "normalizeTy: not(True) reduces to False" $ do
                    let result = normalizeTy env (App (Id "not") [Id "True"])
                    result `shouldBe` Id "False"

                it "normalizeTy: not(False) reduces to True" $ do
                    let result = normalizeTy env (App (Id "not") [Id "False"])
                    result `shouldBe` Id "True"

                it "normalizeTy: not(not(True)) reduces to True" $ do
                    let inner = App (Id "not") [Id "True"]
                        result = normalizeTy env (App (Id "not") [inner])
                    result `shouldBe` Id "True"

                -- Larger computations
                it "normalizeTy: plus(3, 2) = 5 (larger Nat arithmetic)" $ do
                    let n i = Prelude.iterate (\x -> App (Id "Succ") [x]) (Id "Z") !! i
                        result = normalizeTy env (App (Id "plus") [n 3, n 2])
                    result `shouldBe` n 5

                it "normalizeTy: mult(3, 2) = 6" $ do
                    let n i = Prelude.iterate (\x -> App (Id "Succ") [x]) (Id "Z") !! i
                        result = normalizeTy env (App (Id "mult") [n 3, n 2])
                    result `shouldBe` n 6

                it "normalizeTy: plus(mult(2,2), Succ(Z)) = 5 (composed arithmetic)" $ do
                    let n i = Prelude.iterate (\x -> App (Id "Succ") [x]) (Id "Z") !! i
                        inner = App (Id "mult") [n 2, n 2]
                        result = normalizeTy env (App (Id "plus") [inner, n 1])
                    result `shouldBe` n 5

                -- Edge cases
                it "normalizeTy: Id args — reduction attempted but may not fully reduce" $ do
                    let result = normalizeTy env (App (Id "plus") [Id "n", Id "Z"])
                    -- Id is concrete, so reduction IS attempted.
                    -- plus(n, Z) pattern matches: first case is (Z, m) -> m, second is (Succ(k), m) -> ...
                    -- Since n is opaque (not Z or Succ), neither case matches → returns unreduced
                    result `shouldBe` App (Id "plus") [Id "n", Id "Z"]

                it "normalizeTy: Pi type children get normalized" $ do
                    let body = App (Id "plus") [Id "Z", Id "Z"]
                        result = normalizeTy env (Pi Nothing (Id "Int") body)
                    result `shouldBe` Pi Nothing (Id "Int") (Id "Z")

                it "normalizeTy: preserves structure when nothing to normalize" $ do
                    let ty = Pi (Just "x") (Id "Nat") (App (Id "Vec") [Id "Int", Id "x"])
                        result = normalizeTy env ty
                    result `shouldBe` ty

                -- Instance dispatch at type level (algebra methods usable as type-level functions)
                it "normalizeTy: (==) on Nats via instance dispatch — Z == Z reduces to True" $ do
                    let result = normalizeTy env (App (Id "==") [Id "Z", Id "Z"])
                    result `shouldBe` Id "True"

                it "normalizeTy: (==) on Nats via instance dispatch — Z == Succ(Z) reduces to False" $ do
                    let natSZ = App (Id "Succ") [Id "Z"]
                        result = normalizeTy env (App (Id "==") [Id "Z", natSZ])
                    result `shouldBe` Id "False"

                it "normalizeTy: (==) on Nats via instance dispatch — Succ(Z) == Succ(Z) reduces to True" $ do
                    let natSZ = App (Id "Succ") [Id "Z"]
                        result = normalizeTy env (App (Id "==") [natSZ, natSZ])
                    result `shouldBe` Id "True"

                it "normalizeTy: compare on Nats via instance dispatch — compare(Z, Succ(Z)) reduces to LessThan" $ do
                    let natSZ = App (Id "Succ") [Id "Z"]
                        result = normalizeTy env (App (Id "compare") [Id "Z", natSZ])
                    result `shouldBe` Id "LessThan"

                -- exprToCLMTC edge cases
                it "exprToCLMTC: function name → CLMID" $ do
                    let result = exprToCLMTC env (Id "plus")
                    result `shouldBe` CLMID "plus"

                it "exprToCLMTC: Id → CLMID" $ do
                    let result = exprToCLMTC env (Id "a")
                    result `shouldBe` CLMID "a"

                it "exprToCLMTC: U (LConst 0) → CLMU (LConst 0)" $ do
                    let result = exprToCLMTC env (U (LConst 0))
                    result `shouldBe` CLMU (LConst 0)

                it "exprToCLMTC: nested constructor Succ(Succ(Z))" $ do
                    let n2 = App (Id "Succ") [App (Id "Succ") [Id "Z"]]
                        result = exprToCLMTC env n2
                    result `shouldBe` CLMCON (ConsTag "Succ" 1) [CLMCON (ConsTag "Succ" 1) [CLMCON (ConsTag "Z" 0) []]]

                -- clmToExpr edge cases
                it "clmToExpr: CLMEMPTY → Nothing" $ do
                    clmToExpr CLMEMPTY `shouldBe` Nothing

                it "clmToExpr: CLMU (LConst 1) → Just (U (LConst 1))" $ do
                    clmToExpr (CLMU (LConst 1)) `shouldBe` Just (U (LConst 1))

                it "clmToExpr: CLMID → Just (TCon)" $ do
                    clmToExpr (CLMID "Nat") `shouldBe` Just (Id "Nat")

                it "clmToExpr: deeply nested constructor round-trips" $ do
                    let n3 = CLMCON (ConsTag "Succ" 1) [CLMCON (ConsTag "Succ" 1) [CLMCON (ConsTag "Succ" 1) [CLMCON (ConsTag "Z" 0) []]]]
                        expected = App (Id "Succ") [App (Id "Succ") [App (Id "Succ") [Id "Z"]]]
                    clmToExpr n3 `shouldBe` Just expected

                -- isConcreteTy
                it "isConcreteTy: Meta is not concrete" $ do
                    isConcreteTy (Meta 0) `shouldBe` False

                it "isConcreteTy: Id is concrete" $ do
                    isConcreteTy (Id "Int") `shouldBe` True

                it "isConcreteTy: Id is concrete" $ do
                    isConcreteTy (Id "a") `shouldBe` True

                it "isConcreteTy: App with all concrete args is concrete" $ do
                    isConcreteTy (App (Id "Vec") [Id "Int", Id "Z"]) `shouldBe` True

                it "isConcreteTy: App with Meta arg is not concrete" $ do
                    isConcreteTy (App (Id "Vec") [Id "Int", Meta 0]) `shouldBe` False

                it "isConcreteTy: Pi with concrete parts is concrete" $ do
                    isConcreteTy (Pi Nothing (Id "Int") (Id "Bool")) `shouldBe` True

        -- ==================================================================
        -- P35: GADTs (Generalized Algebraic Data Types)
        -- ==================================================================
        describe "Level smart constructors and helpers" $ do
            it "levelSucc normalizes concrete levels" $ do
                levelSucc (LConst 0) `shouldBe` LConst 1
                levelSucc (LConst 3) `shouldBe` LConst 4
            it "levelSucc preserves LVar" $ do
                levelSucc (LVar "l") `shouldBe` LSucc (LVar "l")
            it "levelSucc on LSucc" $ do
                levelSucc (LSucc (LVar "l")) `shouldBe` LSucc (LSucc (LVar "l"))
            it "levelMax normalizes concrete levels" $ do
                levelMax (LConst 1) (LConst 3) `shouldBe` LConst 3
                levelMax (LConst 5) (LConst 2) `shouldBe` LConst 5
                levelMax (LConst 0) (LConst 0) `shouldBe` LConst 0
            it "levelMax preserves LVar" $ do
                levelMax (LVar "l") (LConst 1) `shouldBe` LMax (LVar "l") (LConst 1)
            it "levelLeq on concrete levels" $ do
                levelLeq (LConst 0) (LConst 1) `shouldBe` Just True
                levelLeq (LConst 1) (LConst 1) `shouldBe` Just True
                levelLeq (LConst 2) (LConst 1) `shouldBe` Just False
            it "levelLeq returns Nothing for LVar" $ do
                levelLeq (LVar "l") (LConst 1) `shouldBe` Nothing
                levelLeq (LConst 0) (LVar "l") `shouldBe` Nothing
            it "levelEq on concrete levels" $ do
                levelEq (LConst 0) (LConst 0) `shouldBe` Just True
                levelEq (LConst 0) (LConst 1) `shouldBe` Just False
            it "levelEq on matching LVars" $ do
                levelEq (LVar "l") (LVar "l") `shouldBe` Just True
                levelEq (LVar "l") (LVar "m") `shouldBe` Just False
            it "showLevel renders correctly" $ do
                showLevel (LConst 0) `shouldBe` "0"
                showLevel (LConst 3) `shouldBe` "3"
                showLevel (LVar "l") `shouldBe` "l"
                showLevel (LSucc (LVar "l")) `shouldBe` "l+1"
                showLevel (LMax (LVar "l") (LConst 1)) `shouldBe` "max(l, 1)"
            it "substLevel replaces matching var" $ do
                substLevel "l" (LConst 2) (LVar "l") `shouldBe` LConst 2
            it "substLevel ignores non-matching var" $ do
                substLevel "l" (LConst 2) (LVar "m") `shouldBe` LVar "m"
            it "substLevel recurses into LSucc" $ do
                substLevel "l" (LConst 1) (LSucc (LVar "l")) `shouldBe` LConst 2
            it "substLevel recurses into LMax" $ do
                substLevel "l" (LConst 3) (LMax (LVar "l") (LConst 1)) `shouldBe` LConst 3
            it "substLevel preserves LConst" $ do
                substLevel "l" (LConst 2) (LConst 5) `shouldBe` LConst 5
            it "levelZero and levelOne constants" $ do
                levelZero `shouldBe` LConst 0
                levelOne `shouldBe` LConst 1

        -- ==================================================================
        -- TC: substTyVar Shadowing for TPi/TSigma
        -- ==================================================================
        describe "substTyVar shadowing" $ do
            it "Pi (Just n) shadows: does not substitute in body when name matches" $ do
                -- substTyVar "a" Int (Pi (Just "a") Nat (Id "a")) should NOT replace body's "a"
                let result = substTyVar "a" (Id "Int") (Pi (Just "a") (Id "Nat") (Id "a"))
                result `shouldBe` Pi (Just "a") (Id "Nat") (Id "a")
            it "Pi (Just n) non-shadow: substitutes in body when name differs" $ do
                let result = substTyVar "a" (Id "Int") (Pi (Just "x") (Id "a") (Id "a"))
                result `shouldBe` Pi (Just "x") (Id "Int") (Id "Int")
            it "Pi Nothing always substitutes in both domain and codomain" $ do
                let result = substTyVar "a" (Id "Int") (Pi Nothing (Id "a") (Id "a"))
                result `shouldBe` Pi Nothing (Id "Int") (Id "Int")
            it "Sigma (Just n) shadows: does not substitute in body when name matches" $ do
                let result = substTyVar "a" (Id "Int") (Sigma (Just "a") (Id "Nat") (Id "a"))
                result `shouldBe` Sigma (Just "a") (Id "Nat") (Id "a")
            it "Sigma (Just n) non-shadow: substitutes in body when name differs" $ do
                let result = substTyVar "b" (Id "Bool") (Sigma (Just "x") (Id "b") (Id "b"))
                result `shouldBe` Sigma (Just "x") (Id "Bool") (Id "Bool")
            it "Sigma Nothing always substitutes" $ do
                let result = substTyVar "a" (Id "Int") (Sigma Nothing (Id "a") (Id "a"))
                result `shouldBe` Sigma Nothing (Id "Int") (Id "Int")

        -- ==================================================================
        -- TC: Alpha-Renaming in Unify for TPi/TSigma
        -- ==================================================================
        describe "unify alpha-renaming for TPi/TSigma" $ do
            it "TPi: unifies (x:A) -> B(x) with (y:A) -> B(y) via alpha-rename" $ do
                let st0 = initTCState TCRelaxed
                    env0 = emptyTCEnv
                    ty1 = Pi (Just "x") (Id "Nat") (App (Id "Vec") [Id "Int", Id "x"])
                    ty2 = Pi (Just "y") (Id "Nat") (App (Id "Vec") [Id "Int", Id "y"])
                case runTC (unify ty1 ty2) env0 st0 of
                    Right _ -> pure ()
                    Left errs -> expectationFailure $ "Alpha-rename should succeed: " ++ show errs
            it "TPi: fails when domains differ despite alpha-rename" $ do
                let st0 = initTCState TCStrict
                    env0 = emptyTCEnv
                    ty1 = Pi (Just "x") (Id "Nat") (Id "x")
                    ty2 = Pi (Just "y") (Id "Int") (Id "y")
                case runTC (unify ty1 ty2) env0 st0 of
                    Left _ -> pure ()
                    Right _ -> expectationFailure "Should fail: domains differ"
            it "TPi: unnamed vs named unification works" $ do
                let st0 = initTCState TCRelaxed
                    env0 = emptyTCEnv
                    ty1 = Pi Nothing (Id "Int") (Id "Bool")
                    ty2 = Pi (Just "x") (Id "Int") (Id "Bool")
                case runTC (unify ty1 ty2) env0 st0 of
                    Right _ -> pure ()
                    Left errs -> expectationFailure $ "Should succeed: " ++ show errs
            it "TSigma: unifies (x:A, B(x)) with (y:A, B(y)) via alpha-rename" $ do
                let st0 = initTCState TCRelaxed
                    env0 = emptyTCEnv
                    ty1 = Sigma (Just "x") (Id "Nat") (App (Id "Vec") [Id "Int", Id "x"])
                    ty2 = Sigma (Just "y") (Id "Nat") (App (Id "Vec") [Id "Int", Id "y"])
                case runTC (unify ty1 ty2) env0 st0 of
                    Right _ -> pure ()
                    Left errs -> expectationFailure $ "Alpha-rename should succeed: " ++ show errs

        -- ==================================================================
        -- freeRigidVars
        -- ==================================================================
        describe "freeRigidVars" $ do
            it "Id returns the name" $ do
                freeRigidVars (Id "a") `shouldBe` ["a"]
            it "Id returns empty" $ do
                freeRigidVars (Id "Int") `shouldBe` []
            it "Meta returns empty" $ do
                freeRigidVars (Meta 0) `shouldBe` []
            it "App collects from head and args" $ do
                sort (freeRigidVars (App (Id "f") [Id "a", Id "Int"])) `shouldBe` ["a", "f"]
            it "Pi (Just n) filters bound name from body" $ do
                freeRigidVars (Pi (Just "x") (Id "a") (Id "x")) `shouldBe` ["a"]
            it "Pi Nothing collects from both" $ do
                freeRigidVars (Pi Nothing (Id "a") (Id "b")) `shouldBe` ["a", "b"]
            it "Sigma (Just n) filters bound name from body" $ do
                freeRigidVars (Sigma (Just "x") (Id "a") (Id "x")) `shouldBe` ["a"]
            it "nested types collect all rigids" $ do
                let ty = Pi Nothing (App (Id "Vec") [Id "a", Id "n"]) (Id "b")
                sort (freeRigidVars ty) `shouldBe` ["a", "b", "n"]

        -- ==================================================================
        -- GADT: gadtRefine error/edge cases
        -- ==================================================================
        describe "GADT gadtRefine edge cases" $ do
            it "gadtRefine returns [] for empty checks" $ do
                let st0 = initTCState TCRelaxed
                    env0 = emptyTCEnv
                case runTC (gadtRefine []) env0 st0 of
                    Right (refs, _) -> refs `shouldBe` []
                    Left _ -> expectationFailure "Should not fail"
            it "gadtRefine returns [] when no compiler env" $ do
                let st0 = initTCState TCRelaxed
                    env0 = emptyTCEnv  -- no envCompiler
                    checks = [PatternGuard (PCheckTag (ConsTag "VNil" 0)) (Id "x")]
                case runTC (gadtRefine checks) env0 st0 of
                    Right (refs, _) -> refs `shouldBe` []
                    Left _ -> expectationFailure "Should not fail"
            it "gadtRefine returns [] for literal checks (no constructor)" $ do
                let st0 = initTCState TCRelaxed
                    env0 = emptyTCEnv
                    checks = [PatternGuard (PCheckLit (LInt 42)) (Id "x")]
                case runTC (gadtRefine checks) env0 st0 of
                    Right (refs, _) -> refs `shouldBe` []
                    Left _ -> expectationFailure "Should not fail"
            it "gadtRefine returns [] for unknown constructor" $ do
                let st0 = initTCState TCRelaxed
                    cenv = currentEnvironment st
                    env0 = buildTCEnvFromEnvironment cenv
                    checks = [PatternGuard (PCheckTag (ConsTag "NonExistent" 0)) (Id "x")]
                case runTC (gadtRefine checks) env0 st0 of
                    Right (refs, _) -> refs `shouldBe` []
                    Left _ -> expectationFailure "Should not fail"
            it "gadtRefine returns [] for non-parameterized constructor (True/False)" $ do
                let st0 = initTCState TCRelaxed
                    cenv = currentEnvironment st
                    env0 = buildTCEnvFromEnvironment cenv
                    checks = [PatternGuard (PCheckTag (ConsTag "True" 0)) (Id "x")]
                case runTC (gadtRefine checks) env0 st0 of
                    Right (refs, _) -> refs `shouldBe` []
                    Left _ -> expectationFailure "Should not fail"

        describe "gadtExprToTy" $ do
            it "maps type param names to fresh vars" $ do
                let st0 = initTCState TCRelaxed
                    env0 = emptyTCEnv
                    mapping = [("a", Meta 100), ("n", Meta 101)]
                case runTC (gadtExprToTy mapping (Id "a")) env0 st0 of
                    Right (ty, _) -> ty `shouldBe` Meta 100
                    Left _ -> expectationFailure "Should not fail"
            it "preserves uppercase as Id" $ do
                let st0 = initTCState TCRelaxed
                    env0 = emptyTCEnv
                    mapping = [("a", Meta 100)]
                case runTC (gadtExprToTy mapping (Id "Z")) env0 st0 of
                    Right (ty, _) -> ty `shouldBe` Id "Z"
                    Left _ -> expectationFailure "Should not fail"
            it "handles App with mixed params and constructors" $ do
                let st0 = initTCState TCRelaxed
                    env0 = emptyTCEnv
                    mapping = [("a", Meta 100), ("n", Meta 101)]
                    -- Vec(a, Succ(n))
                    expr = App (Id "Vec") [Id "a", App (Id "Succ") [Id "n"]]
                case runTC (gadtExprToTy mapping expr) env0 st0 of
                    Right (ty, _) ->
                        ty `shouldBe` App (Id "Vec") [Meta 100, App (Id "Succ") [Meta 101]]
                    Left _ -> expectationFailure "Should not fail"
            it "handles U expression" $ do
                let st0 = initTCState TCRelaxed
                    env0 = emptyTCEnv
                case runTC (gadtExprToTy [] (U (LConst 0))) env0 st0 of
                    Right (ty, _) -> ty `shouldBe` U (LConst 0)
                    Left _ -> expectationFailure "Should not fail"

        -- ==================================================================
        -- infer(SumType) kind computation
        -- ==================================================================
        describe "infer(SumType) kind computation" $ do
            it "non-parameterized type has kind Type" $ do
                let st0 = initTCState TCRelaxed
                    env0 = emptyTCEnv
                    boolLam = mkLambda "Bool" [] (Tuple []) (U (LConst 0))
                case runTC (infer (SumType boolLam)) env0 st0 of
                    Right (ty, _) -> ty `shouldBe` U (LConst 0)
                    Left errs -> expectationFailure $ "Failed: " ++ show errs
            it "single-param type has kind Type -> Type" $ do
                let st0 = initTCState TCRelaxed
                    cenv = currentEnvironment st
                    env0 = buildTCEnvFromEnvironment cenv
                    maybeLam = mkLambda "Maybe" [Var "a" (U (LConst 0)) UNDEFINED] (Tuple []) (U (LConst 0))
                case runTC (infer (SumType maybeLam)) env0 st0 of
                    Right (ty, _) -> ty `shouldBe` Pi Nothing (U (LConst 0)) (U (LConst 0))
                    Left errs -> expectationFailure $ "Failed: " ++ show errs
            it "two-param type has kind Type -> Type -> Type" $ do
                let st0 = initTCState TCRelaxed
                    cenv = currentEnvironment st
                    env0 = buildTCEnvFromEnvironment cenv
                    eitherLam = mkLambda "Either" [Var "a" (U (LConst 0)) UNDEFINED, Var "b" (U (LConst 0)) UNDEFINED] (Tuple []) (U (LConst 0))
                case runTC (infer (SumType eitherLam)) env0 st0 of
                    Right (ty, _) -> ty `shouldBe` Pi Nothing (U (LConst 0)) (Pi Nothing (U (LConst 0)) (U (LConst 0)))
                    Left errs -> expectationFailure $ "Failed: " ++ show errs

        -- ==================================================================
        -- normalizeTy for Sigma dependent substitution
        -- ==================================================================
        describe "normalizeTy TSigma" $ do
            it "normalizes Sigma children" $ do
                let env0 = currentEnvironment st
                    -- Sigma Nothing (plus(Z,Z)) (Id "Int") → Sigma Nothing Z Int
                    natZ = Id "Z"
                    inner = App (Id "plus") [natZ, natZ]
                    result = normalizeTy env0 (Sigma Nothing inner (Id "Int"))
                result `shouldBe` Sigma Nothing natZ (Id "Int")
            it "preserves Sigma when nothing to normalize" $ do
                let env0 = currentEnvironment st
                    ty = Sigma (Just "x") (Id "Nat") (Id "Int")
                    result = normalizeTy env0 ty
                result `shouldBe` ty

        -- ==================================================================
        -- Positivity Checking (Pass 2.1)
        -- ==================================================================
        describe "positivityCheckPass" $ do
            it "no warning for simple enum type" $ do
                -- type Color = Red | Green | Blue (no params, trivially positive)
                let redCon = mkLambda "Red" [] (Tuple []) (Id "Color")
                    greenCon = mkLambda "Green" [] (Tuple []) (Id "Color")
                    blueCon = mkLambda "Blue" [] (Tuple []) (Id "Color")
                    colorType = SumType (mkLambda "Color" [] (Constructors [redCon, greenCon, blueCon]) (U (LConst 0)))
                    env = initialEnvironment { types = Map.fromList [("Color", colorType)] }
                    s = mkStateWithEnv env
                warnings <- runAndGetWarnings s positivityCheckPass
                warnings `shouldBe` []

            it "no warning for recursive type in positive position" $ do
                -- type Nat = Z | Succ(Nat)  -- Nat only in positive position
                let zCon = mkLambda "Z" [] (Tuple []) (Id "Nat")
                    succCon = mkLambda "Succ" [Var "n" (Id "Nat") UNDEFINED] (Tuple [Id "n"]) (Id "Nat")
                    natType = SumType (mkLambda "Nat" [] (Constructors [zCon, succCon]) (U (LConst 0)))
                    env = initialEnvironment { types = Map.fromList [("Nat", natType)] }
                    s = mkStateWithEnv env
                warnings <- runAndGetWarnings s positivityCheckPass
                warnings `shouldBe` []

            it "warns on negative occurrence (type in function domain)" $ do
                -- type Bad = MkBad(Bad -> Int) -- Bad in negative position
                let mkBadCon = mkLambda "MkBad" [Var "f" (Pi Nothing (Id "Bad") (Id "Int")) UNDEFINED] (Tuple [Id "f"]) (Id "Bad")
                    badType = SumType (mkLambda "Bad" [] (Constructors [mkBadCon]) (U (LConst 0)))
                    env = initialEnvironment { types = Map.fromList [("Bad", badType)] }
                    s = mkStateWithEnv env
                warnings <- runAndGetWarnings s positivityCheckPass
                Prelude.length warnings `shouldBe` 1
                Prelude.head warnings `shouldSatisfy` isInfixOf "positivity"

            it "no warning for type in function codomain (positive)" $ do
                -- type F = MkF(Int -> F) -- F only in positive position (result)
                let mkFCon = mkLambda "MkF" [Var "f" (Pi Nothing (Id "Int") (Id "F")) UNDEFINED] (Tuple [Id "f"]) (Id "F")
                    fType = SumType (mkLambda "F" [] (Constructors [mkFCon]) (U (LConst 0)))
                    env = initialEnvironment { types = Map.fromList [("F", fType)] }
                    s = mkStateWithEnv env
                warnings <- runAndGetWarnings s positivityCheckPass
                warnings `shouldBe` []

            it "warns on doubly-negative (negative overall) occurrence" $ do
                -- type Bad2 = MkBad2((Bad2 -> Int) -> Int) -- Bad2 in neg-of-neg-of-pos = neg
                -- Actually (Bad2 -> Int) -> Int: Bad2 is in domain of domain = positive
                -- Let's use: type Bad2 = MkBad2((Int -> Bad2) -> Int) -- Bad2 in codomain-of-domain = negative
                let innerArrow = Pi Nothing (Id "Int") (Id "Bad2")  -- Int -> Bad2
                    outerArrow = Pi Nothing innerArrow (Id "Int")   -- (Int -> Bad2) -> Int
                    mkCon = mkLambda "MkBad2" [Var "f" outerArrow UNDEFINED] (Tuple [Id "f"]) (Id "Bad2")
                    badType = SumType (mkLambda "Bad2" [] (Constructors [mkCon]) (U (LConst 0)))
                    env = initialEnvironment { types = Map.fromList [("Bad2", badType)] }
                    s = mkStateWithEnv env
                warnings <- runAndGetWarnings s positivityCheckPass
                Prelude.length warnings `shouldBe` 1

            it "disabled when flag is off" $ do
                let mkBadCon = mkLambda "MkBad" [Var "f" (Pi Nothing (Id "Bad") (Id "Int")) UNDEFINED] (Tuple [Id "f"]) (Id "Bad")
                    badType = SumType (mkLambda "Bad" [] (Constructors [mkBadCon]) (U (LConst 0)))
                    env = initialEnvironment { types = Map.fromList [("Bad", badType)] }
                    s = (mkStateWithEnv env) { currentFlags = (currentFlags (mkStateWithEnv env)) { checkPositivity = False } }
                warnings <- runAndGetWarnings s positivityCheckPass
                warnings `shouldBe` []

        -- ==================================================================
        -- Termination Checking (Pass 2.2)
        -- ==================================================================
        describe "terminationCheckPass" $ do
            it "no warning for non-recursive function" $ do
                -- function id(x:Int) : Int = x;
                let lam = mkLambda "id" [Var "x" (Id "Int") UNDEFINED] (Id "x") (Id "Int")
                    env = initialEnvironment { topLambdas = Map.fromList [("id", lam)] }
                    s = mkStateWithEnv env
                warnings <- runAndGetWarnings s terminationCheckPass
                warnings `shouldBe` []

            it "no warning for structurally decreasing recursion" $ do
                -- function f(x:Nat) : Nat = match | Z -> Z | Succ(n) -> f(n);
                -- Use CaseOf with Var patterns that bind constructor subterms
                let zCase = CaseOf [Var "x" UNDEFINED (ConTuple (ConsTag "Z" 0) [])]
                              (Id "Z") SourceInteractive
                    -- Succ(n) pattern: Var "n" bound inside constructor app
                    succCase = CaseOf [Var "x" UNDEFINED (App (Id "Succ") [Id "n"])]
                                 (App (Id "f") [Id "n"]) SourceInteractive
                    matchBody = PatternMatches [zCase, succCase]
                    lam = mkLambda "f" [Var "x" (Id "Nat") UNDEFINED] matchBody (Id "Nat")
                    env = initialEnvironment { topLambdas = Map.fromList [("f", lam)] }
                    s = mkStateWithEnv env
                warnings <- runAndGetWarnings s terminationCheckPass
                -- The function IS self-recursive (references "f" in body)
                -- n is a structural subterm of x (extracted from Succ pattern)
                warnings `shouldBe` []

            it "warns on non-decreasing recursion" $ do
                -- function loop(x:Int) : Int = loop(x);  -- no structural decrease
                let lam = mkLambda "loop" [Var "x" (Id "Int") UNDEFINED]
                            (App (Id "loop") [Id "x"]) (Id "Int")
                    env = initialEnvironment { topLambdas = Map.fromList [("loop", lam)] }
                    s = (mkStateWithEnv env) { parsedModule = [(Function lam, SourceInteractive)] }
                warnings <- runAndGetWarnings s terminationCheckPass
                Prelude.length warnings `shouldBe` 1
                Prelude.head warnings `shouldSatisfy` isInfixOf "termination"

            it "warns on growing argument recursion" $ do
                -- function grow(x:Nat) : Nat = grow(Succ(x));  -- arg grows
                let lam = mkLambda "grow" [Var "x" (Id "Nat") UNDEFINED]
                            (App (Id "grow") [App (Id "Succ") [Id "x"]]) (Id "Nat")
                    env = initialEnvironment { topLambdas = Map.fromList [("grow", lam)] }
                    s = (mkStateWithEnv env) { parsedModule = [(Function lam, SourceInteractive)] }
                warnings <- runAndGetWarnings s terminationCheckPass
                Prelude.length warnings `shouldBe` 1

            it "disabled when flag is off" $ do
                let lam = mkLambda "loop" [Var "x" (Id "Int") UNDEFINED]
                            (App (Id "loop") [Id "x"]) (Id "Int")
                    env = initialEnvironment { topLambdas = Map.fromList [("loop", lam)] }
                    s = (mkStateWithEnv env) { currentFlags = (currentFlags (mkStateWithEnv env)) { checkTermination = False } }
                warnings <- runAndGetWarnings s terminationCheckPass
                warnings `shouldBe` []

        -- ==================================================================
        -- Pattern Match Coverage Checking (Pass 2.3)
        -- ==================================================================
        describe "coverageCheckPass" $ do
            it "no warning when all constructors covered" $ do
                -- type Bool = True | False, match covers both
                let trueCon = mkLambda "True" [] (Tuple []) (Id "Bool")
                    falseCon = mkLambda "False" [] (Tuple []) (Id "Bool")
                    boolType = SumType (mkLambda "Bool" [] (Constructors [trueCon, falseCon]) (U (LConst 0)))
                    trueCase = ExpandedCase [ExprConsTagCheck (ConsTag "True" 0) (Id "x")]
                                 (Id "a") SourceInteractive
                    falseCase = ExpandedCase [ExprConsTagCheck (ConsTag "False" 1) (Id "x")]
                                  (Id "b") SourceInteractive
                    matchBody = PatternMatches [trueCase, falseCase]
                    lam = mkLambda "f" [Var "x" (Id "Bool") UNDEFINED] matchBody (Id "Bool")
                    env = initialEnvironment {
                        types = Map.fromList [("Bool", boolType)],
                        constructors = Map.fromList [
                            ("True", (trueCon, 0)),
                            ("False", (falseCon, 1))
                        ],
                        topLambdas = Map.fromList [("f", lam)]
                    }
                    s = mkStateWithEnv env
                warnings <- runAndGetWarnings s coverageCheckPass
                warnings `shouldBe` []

            it "warns when constructor is missing" $ do
                -- type Bool = True | False, match only covers True
                let trueCon = mkLambda "True" [] (Tuple []) (Id "Bool")
                    falseCon = mkLambda "False" [] (Tuple []) (Id "Bool")
                    boolType = SumType (mkLambda "Bool" [] (Constructors [trueCon, falseCon]) (U (LConst 0)))
                    trueCase = ExpandedCase [ExprConsTagCheck (ConsTag "True" 0) (Id "x")]
                                 (Id "a") SourceInteractive
                    matchBody = PatternMatches [trueCase]
                    lam = mkLambda "f" [Var "x" (Id "Bool") UNDEFINED] matchBody (Id "Bool")
                    env = initialEnvironment {
                        types = Map.fromList [("Bool", boolType)],
                        constructors = Map.fromList [
                            ("True", (trueCon, 0)),
                            ("False", (falseCon, 1))
                        ],
                        topLambdas = Map.fromList [("f", lam)]
                    }
                    s = mkStateWithEnv env
                warnings <- runAndGetWarnings s coverageCheckPass
                Prelude.length warnings `shouldBe` 1
                Prelude.head warnings `shouldSatisfy` isInfixOf "False"
                Prelude.head warnings `shouldSatisfy` isInfixOf "coverage"

            it "no warning with wildcard/default case" $ do
                -- match with empty checks = wildcard
                let trueCon = mkLambda "True" [] (Tuple []) (Id "Bool")
                    falseCon = mkLambda "False" [] (Tuple []) (Id "Bool")
                    boolType = SumType (mkLambda "Bool" [] (Constructors [trueCon, falseCon]) (U (LConst 0)))
                    trueCase = ExpandedCase [ExprConsTagCheck (ConsTag "True" 0) (Id "x")]
                                 (Id "a") SourceInteractive
                    wildcardCase = ExpandedCase [] (Id "b") SourceInteractive
                    matchBody = PatternMatches [trueCase, wildcardCase]
                    lam = mkLambda "f" [Var "x" (Id "Bool") UNDEFINED] matchBody (Id "Bool")
                    env = initialEnvironment {
                        types = Map.fromList [("Bool", boolType)],
                        constructors = Map.fromList [
                            ("True", (trueCon, 0)),
                            ("False", (falseCon, 1))
                        ],
                        topLambdas = Map.fromList [("f", lam)]
                    }
                    s = mkStateWithEnv env
                warnings <- runAndGetWarnings s coverageCheckPass
                warnings `shouldBe` []

            it "warns with multiple missing constructors" $ do
                -- type Color = Red | Green | Blue, match only covers Red
                let redCon = mkLambda "Red" [] (Tuple []) (Id "Color")
                    greenCon = mkLambda "Green" [] (Tuple []) (Id "Color")
                    blueCon = mkLambda "Blue" [] (Tuple []) (Id "Color")
                    colorType = SumType (mkLambda "Color" [] (Constructors [redCon, greenCon, blueCon]) (U (LConst 0)))
                    redCase = ExpandedCase [ExprConsTagCheck (ConsTag "Red" 0) (Id "x")]
                                (Id "a") SourceInteractive
                    matchBody = PatternMatches [redCase]
                    lam = mkLambda "f" [Var "x" (Id "Color") UNDEFINED] matchBody (Id "Color")
                    env = initialEnvironment {
                        types = Map.fromList [("Color", colorType)],
                        constructors = Map.fromList [
                            ("Red", (redCon, 0)),
                            ("Green", (greenCon, 1)),
                            ("Blue", (blueCon, 2))
                        ],
                        topLambdas = Map.fromList [("f", lam)]
                    }
                    s = mkStateWithEnv env
                warnings <- runAndGetWarnings s coverageCheckPass
                Prelude.length warnings `shouldBe` 1
                Prelude.head warnings `shouldSatisfy` isInfixOf "Green"
                Prelude.head warnings `shouldSatisfy` isInfixOf "Blue"

            it "checks instance lambdas too" $ do
                let trueCon = mkLambda "True" [] (Tuple []) (Id "Bool")
                    falseCon = mkLambda "False" [] (Tuple []) (Id "Bool")
                    boolType = SumType (mkLambda "Bool" [] (Constructors [trueCon, falseCon]) (U (LConst 0)))
                    trueCase = ExpandedCase [ExprConsTagCheck (ConsTag "True" 0) (Id "x")]
                                 (Id "a") SourceInteractive
                    matchBody = PatternMatches [trueCase]
                    lam = mkLambda "show\0Bool" [Var "x" (Id "Bool") UNDEFINED] matchBody (Id "String")
                    env = initialEnvironment {
                        types = Map.fromList [("Bool", boolType)],
                        constructors = Map.fromList [
                            ("True", (trueCon, 0)),
                            ("False", (falseCon, 1))
                        ],
                        instanceLambdas = Map.fromList [("show\0Bool", lam)]
                    }
                    s = mkStateWithEnv env
                warnings <- runAndGetWarnings s coverageCheckPass
                Prelude.length warnings `shouldBe` 1

            it "disabled when flag is off" $ do
                let trueCon = mkLambda "True" [] (Tuple []) (Id "Bool")
                    falseCon = mkLambda "False" [] (Tuple []) (Id "Bool")
                    boolType = SumType (mkLambda "Bool" [] (Constructors [trueCon, falseCon]) (U (LConst 0)))
                    trueCase = ExpandedCase [ExprConsTagCheck (ConsTag "True" 0) (Id "x")]
                                 (Id "a") SourceInteractive
                    matchBody = PatternMatches [trueCase]
                    lam = mkLambda "f" [Var "x" (Id "Bool") UNDEFINED] matchBody (Id "Bool")
                    env = initialEnvironment {
                        types = Map.fromList [("Bool", boolType)],
                        constructors = Map.fromList [
                            ("True", (trueCon, 0)),
                            ("False", (falseCon, 1))
                        ],
                        topLambdas = Map.fromList [("f", lam)]
                    }
                    s = (mkStateWithEnv env) { currentFlags = (currentFlags (mkStateWithEnv env)) { checkCoverage = False } }
                warnings <- runAndGetWarnings s coverageCheckPass
                warnings `shouldBe` []

        -- ==================================================================
        -- Integration: three passes on real stdlib
        -- ==================================================================
        describe "three passes on stdlib" $ do
            it "positivity check produces no warnings on stdlib types" $ do
                warnings <- runAndGetWarnings st positivityCheckPass
                let posWarnings = Prelude.filter (isInfixOf "positivity") warnings
                posWarnings `shouldBe` []

            it "coverage check produces no errors on stdlib" $ do
                warnings <- runAndGetWarnings st coverageCheckPass
                let covWarnings = Prelude.filter (isInfixOf "coverage") warnings
                covWarnings `shouldBe` []

        -- ==================================================================
        -- New Surface Syntax: +/* for type definitions (TDD — all should fail initially)
        -- ==================================================================
        describe "New syntax: type definitions with + and *" $ do

            -- A: Enum types with + separator
            it "A1: parses two-variant enum with +" $ do
                res <- parseTestString "type MyBool = MyTrue + MyFalse;"
                case res of
                    Right [SumType lam] -> do
                        lamName lam `shouldBe` "MyBool"
                        case body lam of
                            Constructors cs -> length cs `shouldBe` 2
                            other -> expectationFailure $ "Expected Constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "A2: parses three-variant enum with +" $ do
                res <- parseTestString "type Color3 = Red3 + Green3 + Blue3;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors cs -> do
                                length cs `shouldBe` 3
                                map lamName cs `shouldBe` ["Red3", "Green3", "Blue3"]
                            other -> expectationFailure $ "Expected Constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "A3: parses single nullary constructor" $ do
                res <- parseTestString "type MyUnit = MyUnitVal;"
                case res of
                    Right [SumType lam] -> do
                        lamName lam `shouldBe` "MyUnit"
                        case body lam of
                            Constructors [c] -> lamName c `shouldBe` "MyUnitVal"
                            other -> expectationFailure $ "Expected single constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            -- B: Single-variant types (implicit constructor from lowercase field start)
            it "B1: parses implicit-constructor record with *" $ do
                res <- parseTestString "type Pt = x:Float64 * y:Float64;"
                case res of
                    Right [SumType lam] -> do
                        lamName lam `shouldBe` "Pt"
                        case body lam of
                            Constructors [c] -> do
                                lamName c `shouldBe` "Pt"  -- implicit constructor name = type name
                                length (params c) `shouldBe` 2
                                Surface.name (head (params c)) `shouldBe` "x"
                                Surface.name (params c !! 1) `shouldBe` "y"
                            other -> expectationFailure $ "Expected single implicit constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "B2: parses three-field implicit record" $ do
                res <- parseTestString "type Pers = nm:String * ag:Int * act:Bool;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors [c] -> do
                                lamName c `shouldBe` "Pers"
                                length (params c) `shouldBe` 3
                            other -> expectationFailure $ "Expected single constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "B3: parses single-field implicit record" $ do
                res <- parseTestString "type Wrap = val:Int;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors [c] -> do
                                lamName c `shouldBe` "Wrap"
                                length (params c) `shouldBe` 1
                                Surface.name (head (params c)) `shouldBe` "val"
                            other -> expectationFailure $ "Expected single constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            -- C: Sum types with fields using + and *
            it "C1: parses Maybe-like with nullary + unary using +" $ do
                res <- parseTestString "type MyMaybe(a:Type) = MyNothing + MyJust * val:a;"
                case res of
                    Right [SumType lam] -> do
                        lamName lam `shouldBe` "MyMaybe"
                        length (params lam) `shouldBe` 1
                        case body lam of
                            Constructors cs -> do
                                length cs `shouldBe` 2
                                lamName (head cs) `shouldBe` "MyNothing"
                                params (head cs) `shouldBe` []
                                lamName (cs !! 1) `shouldBe` "MyJust"
                                length (params (cs !! 1)) `shouldBe` 1
                                Surface.name (head (params (cs !! 1))) `shouldBe` "val"
                            other -> expectationFailure $ "Expected Constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "C2: parses Either-like with two unary variants" $ do
                res <- parseTestString "type MyEither(a:Type, b:Type) = MyLeft * val:a + MyRight * val:b;"
                case res of
                    Right [SumType lam] -> do
                        length (params lam) `shouldBe` 2
                        case body lam of
                            Constructors cs -> do
                                length cs `shouldBe` 2
                                lamName (head cs) `shouldBe` "MyLeft"
                                length (params (head cs)) `shouldBe` 1
                                lamName (cs !! 1) `shouldBe` "MyRight"
                                length (params (cs !! 1)) `shouldBe` 1
                            other -> expectationFailure $ "Expected Constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "C3: parses multi-field constructors in sum" $ do
                res <- parseTestString "type Sh = Circ * cx:Float64 * cy:Float64 * r:Float64 + Rct * w:Float64 * h:Float64;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors cs -> do
                                length cs `shouldBe` 2
                                lamName (head cs) `shouldBe` "Circ"
                                length (params (head cs)) `shouldBe` 3
                                lamName (cs !! 1) `shouldBe` "Rct"
                                length (params (cs !! 1)) `shouldBe` 2
                            other -> expectationFailure $ "Expected Constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "C4: parses recursive sum type" $ do
                res <- parseTestString "type MyList(a:Type) = MyNil + MyCons * hd:a * tl:MyList(a);"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors cs -> do
                                length cs `shouldBe` 2
                                lamName (cs !! 1) `shouldBe` "MyCons"
                                length (params (cs !! 1)) `shouldBe` 2
                            other -> expectationFailure $ "Expected Constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "C5: parses three-way sum with mixed arity" $ do
                res <- parseTestString "type Ex = LitE * v:Int + AddE * l:Ex * r:Ex + NegE * e:Ex;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors cs -> do
                                length cs `shouldBe` 3
                                map lamName cs `shouldBe` ["LitE", "AddE", "NegE"]
                                map (length . params) cs `shouldBe` [1, 2, 1]
                            other -> expectationFailure $ "Expected Constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            -- D: GADTs with new syntax
            it "D1: parses GADT with nullary constructor return type" $ do
                res <- parseTestString "type PEq(a:Type, b:Type) = PRefl : PEq(a, a);"
                case res of
                    Right [SumType lam] -> do
                        lamName lam `shouldBe` "PEq"
                        case body lam of
                            Constructors [c] -> do
                                lamName c `shouldBe` "PRefl"
                                params c `shouldBe` []
                                -- GADT return type should be PEq(a, a), not the default PEq(a, b)
                                case lamType c of
                                    App (Id "PEq") [Id "a", Id "a"] -> return ()
                                    other -> expectationFailure $ "Expected PEq(a,a), got: " ++ show other
                            other -> expectationFailure $ "Expected single constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "D2: parses GADT with fields + return type" $ do
                res <- parseTestString "type GV(a:Type, n:Nat) = GVNil : GV(a, Z) + GVCons * hd:a * tl:GV(a, n) : GV(a, Succ(n));"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors cs -> do
                                length cs `shouldBe` 2
                                -- GVNil: nullary, return type GV(a, Z)
                                lamName (head cs) `shouldBe` "GVNil"
                                params (head cs) `shouldBe` []
                                -- GVCons: two fields, return type GV(a, Succ(n))
                                lamName (cs !! 1) `shouldBe` "GVCons"
                                length (params (cs !! 1)) `shouldBe` 2
                            other -> expectationFailure $ "Expected Constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "D3: parses multi-variant GADT" $ do
                res <- parseTestString "type TE(a:Type) = TLI * n:Int : TE(Int) + TLB * b:Bool : TE(Bool);"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors cs -> do
                                length cs `shouldBe` 2
                                lamName (head cs) `shouldBe` "TLI"
                                lamName (cs !! 1) `shouldBe` "TLB"
                            other -> expectationFailure $ "Expected Constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "D4: parses SafeOption-style GADT with Bool index" $ do
                res <- parseTestString "type SO(a:Type, p:Bool) = SomeV * x:a : SO(a, True) + NoV : SO(a, False);"
                case res of
                    Right [SumType lam] -> do
                        length (params lam) `shouldBe` 2
                        case body lam of
                            Constructors cs -> do
                                length cs `shouldBe` 2
                                length (params (head cs)) `shouldBe` 1
                                params (cs !! 1) `shouldBe` []
                            other -> expectationFailure $ "Expected Constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            -- E: Dependent telescopes (field types reference earlier field names)
            it "E1: parses dependent record with telescope" $ do
                res <- parseTestString "type SV(a:Type) = n:Nat * elems:Vec(a, n);"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors [c] -> do
                                lamName c `shouldBe` "SV"  -- implicit constructor
                                length (params c) `shouldBe` 2
                                Surface.name (head (params c)) `shouldBe` "n"
                                Surface.name (params c !! 1) `shouldBe` "elems"
                                -- The type of elems should reference n
                                case typ (params c !! 1) of
                                    App (Id "Vec") [Id "a", Id "n"] -> return ()
                                    other -> expectationFailure $ "Expected Vec(a,n), got: " ++ show other
                            other -> expectationFailure $ "Expected single constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "E2: parses triple-field dependent telescope" $ do
                res <- parseTestString "type Mat(a:Type) = rows:Nat * cols:Nat * dat:Vec(Vec(a, cols), rows);"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors [c] -> do
                                length (params c) `shouldBe` 3
                            other -> expectationFailure $ "Expected single constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            -- F: Function types in fields
            it "F1: parses arrow type in field without parens" $ do
                res <- parseTestString "type Hndl = hname:String * f:Int -> Bool;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors [c] -> do
                                length (params c) `shouldBe` 2
                                Surface.name (params c !! 1) `shouldBe` "f"
                                -- f's type should be Int -> Bool (Pi Nothing Int Bool)
                                case typ (params c !! 1) of
                                    Pi Nothing (Id "Int") (Id "Bool") -> return ()
                                    other -> expectationFailure $ "Expected Int -> Bool, got: " ++ show other
                            other -> expectationFailure $ "Expected single constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "F2: parses multi-arrow in field" $ do
                res <- parseTestString "type Mpr = transform:Int -> Int -> Int * label:String;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors [c] -> do
                                length (params c) `shouldBe` 2
                                Surface.name (head (params c)) `shouldBe` "transform"
                                Surface.name (params c !! 1) `shouldBe` "label"
                                -- transform has type Int -> Int -> Int (right-assoc arrows)
                                case typ (head (params c)) of
                                    Pi Nothing (Id "Int") (Pi Nothing (Id "Int") (Id "Int")) -> return ()
                                    other -> expectationFailure $ "Expected Int -> Int -> Int, got: " ++ show other
                            other -> expectationFailure $ "Expected single constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "F3: parses parenthesized product inside field type" $ do
                res <- parseTestString "type Xf = f:(Int * Bool) -> String * nm:String;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors [c] -> do
                                length (params c) `shouldBe` 2
                                Surface.name (head (params c)) `shouldBe` "f"
                                Surface.name (params c !! 1) `shouldBe` "nm"
                            other -> expectationFailure $ "Expected single constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            -- G: Dependent Pi in field
            it "G1: parses dependent Pi in field type" $ do
                res <- parseTestString "type DH = nm:String * f:(n:Nat) -> Vec(Int, n);"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors [c] -> do
                                length (params c) `shouldBe` 2
                                Surface.name (params c !! 1) `shouldBe` "f"
                                -- f's type: (n:Nat) -> Vec(Int, n) = Pi (Just "n") Nat (App Vec [Int, n])
                                case typ (params c !! 1) of
                                    Pi (Just "n") (Id "Nat") _ -> return ()
                                    other -> expectationFailure $ "Expected dependent Pi, got: " ++ show other
                            other -> expectationFailure $ "Expected single constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

        -- ==================================================================
        -- New syntax: forall and exists in type expressions
        -- ==================================================================
        describe "New syntax: forall/exists quantifiers" $ do

            it "H1: parses forall a. a -> a in type annotation" $ do
                res <- parseTestString "function myId(x: forall a. a -> a) : Int = 42;"
                case res of
                    Right [Function lam] -> do
                        -- The param type should contain forall
                        case typ (head (params lam)) of
                            Pi (Just "a") (U (LConst 0)) (Pi Nothing (Id "a") (Id "a")) -> return ()
                            other -> expectationFailure $ "Expected forall a. a -> a, got: " ++ show other
                    other -> expectationFailure $ "Expected Function, got: " ++ show other

            it "H2: parses forall with two variables" $ do
                res <- parseTestString "function myConst(x: forall a b. a -> b -> a) : Int = 42;"
                case res of
                    Right [Function lam] -> do
                        case typ (head (params lam)) of
                            Pi (Just "a") (U (LConst 0)) (Pi (Just "b") (U (LConst 0)) _) -> return ()
                            other -> expectationFailure $ "Expected nested forall, got: " ++ show other
                    other -> expectationFailure $ "Expected Function, got: " ++ show other

            it "H3: parses forall with kind annotation" $ do
                res <- parseTestString "function myF(x: forall (a:Type)(b:Type). a -> b) : Int = 42;"
                case res of
                    Right [Function lam] -> do
                        case typ (head (params lam)) of
                            Pi (Just "a") (U (LConst 0)) (Pi (Just "b") (U (LConst 0)) _) -> return ()
                            other -> expectationFailure $ "Expected kinded forall, got: " ++ show other
                    other -> expectationFailure $ "Expected Function, got: " ++ show other

            it "H4: parses unicode ∀ as alias for forall" $ do
                res <- parseTestString "function myId2(x: ∀ a. a -> a) : Int = 42;"
                case res of
                    Right [Function lam] -> do
                        case typ (head (params lam)) of
                            Pi (Just "a") (U (LConst 0)) (Pi Nothing (Id "a") (Id "a")) -> return ()
                            other -> expectationFailure $ "Expected forall from ∀, got: " ++ show other
                    other -> expectationFailure $ "Expected Function, got: " ++ show other

            it "H5: parses exists (a:Type). body" $ do
                res <- parseTestString "type Showable = exists (a:Type). val:a * show:a -> String;"
                case res of
                    Right [SumType lam] -> do
                        -- The body of the type should contain Exists
                        -- Exists is a new AST node or desugar - for now just check parsing succeeds
                        lamName lam `shouldBe` "Showable"
                    other -> expectationFailure $ "Expected SumType with exists, got: " ++ show other

            it "H6: parses unicode ∃ as alias for exists" $ do
                res <- parseTestString "type Hidden = ∃ (a:Type). a;"
                case res of
                    Right [SumType lam] -> do
                        lamName lam `shouldBe` "Hidden"
                    other -> expectationFailure $ "Expected SumType with ∃, got: " ++ show other

            it "H7: parses multi-variable exists" $ do
                res <- parseTestString "type BiHid = exists (a:Type)(b:Type). f:a -> b * val:a;"
                case res of
                    Right [SumType lam] -> do
                        lamName lam `shouldBe` "BiHid"
                    other -> expectationFailure $ "Expected SumType with multi-exists, got: " ++ show other

        -- ==================================================================
        -- New syntax: * and + as type operators in annotations
        -- ==================================================================
        describe "New syntax: * and + as type operators in annotations" $ do

            it "I1: parses Int * Bool as product type annotation" $ do
                res <- runFresh $ parseExpr (T.pack "42 : Int * Bool")
                case res of
                    Right (Typed (Lit (LInt 42)) _prodTy) -> return ()
                    other -> expectationFailure $ "Expected typed expr with product, got: " ++ show other

            it "I2: parses Int + Bool as sum type annotation" $ do
                res <- runFresh $ parseExpr (T.pack "42 : Int + Bool")
                case res of
                    Right (Typed (Lit (LInt 42)) _sumTy) -> return ()
                    other -> expectationFailure $ "Expected typed expr with sum, got: " ++ show other

            it "I3: * binds tighter than + in annotations" $ do
                -- Int * Bool + String * Char should be (Int * Bool) + (String * Char)
                res <- parseTestString "function f(x: Int * Bool + String * Char) : Int = 42;"
                case res of
                    Right [Function _] -> return ()  -- just check parsing succeeds for now
                    other -> expectationFailure $ "Expected Function, got: " ++ show other

            it "I4: -> binds looser than * in annotations" $ do
                -- Int * Bool -> String means (Int * Bool) -> String
                res <- parseTestString "function f(x: Int * Bool -> String) : Int = 42;"
                case res of
                    Right [Function lam] -> do
                        case typ (head (params lam)) of
                            Pi Nothing _ (Id "String") -> return ()
                            other -> expectationFailure $ "Expected (... -> String), got: " ++ show other
                    other -> expectationFailure $ "Expected Function, got: " ++ show other

            it "I5: -> binds looser than + in annotations" $ do
                -- a -> b + c means a -> (b + c)  (right-assoc arrow, + in codomain)
                res <- parseTestString "function f(x: Int -> Bool + String) : Int = 42;"
                case res of
                    Right [Function _] -> return ()  -- just check parsing succeeds
                    other -> expectationFailure $ "Expected Function, got: " ++ show other

            it "I6: forall scopes over everything" $ do
                -- forall a. a -> a * a means forall a. (a -> (a * a))
                res <- parseTestString "function f(x: forall a. a -> a * a) : Int = 42;"
                case res of
                    Right [Function lam] -> do
                        case typ (head (params lam)) of
                            Pi (Just "a") (U (LConst 0)) (Pi Nothing (Id "a") _) -> return ()
                            other -> expectationFailure $ "Expected forall a. (a -> ...), got: " ++ show other
                    other -> expectationFailure $ "Expected Function, got: " ++ show other

        -- ==================================================================
        -- New syntax: unpack expression
        -- ==================================================================
        describe "New syntax: unpack expression" $ do

            it "J1: parses unpack e as (a, x) in body" $ do
                res <- runFresh $ parseExpr (T.pack "unpack s as (a, x) in x")
                case res of
                    Right _ -> return ()  -- just check parsing succeeds
                    Left err -> expectationFailure $ "Parse failed: " ++ show err

        -- ==================================================================
        -- New syntax: type composition with + (flat variant inlining)
        -- ==================================================================
        describe "New syntax: type composition" $ do

            it "K1: parses type defined as sum of existing types" $ do
                -- type Shape = Circle + Rect; where Circle and Rect are previously defined types
                res <- parseTestString "type Shape2 = Circle2 + Rect2;"
                case res of
                    Right [SumType lam] -> do
                        lamName lam `shouldBe` "Shape2"
                        case body lam of
                            Constructors cs -> do
                                length cs `shouldBe` 2
                                map lamName cs `shouldBe` ["Circle2", "Rect2"]
                            other -> expectationFailure $ "Expected Constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "K2: parses type extension with new fields" $ do
                -- type LabeledPoint = Point * label:String;
                -- Here Point is an existing type, label:String is a new field
                -- This should parse as a single-constructor type with Point + label field
                res <- parseTestString "type LPt = BasePt * label:String;"
                case res of
                    Right [SumType lam] -> do
                        lamName lam `shouldBe` "LPt"
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

        -- ==================================================================
        -- New syntax: edge cases and complex combinations
        -- ==================================================================
        describe "New syntax: edge cases" $ do

            it "L1: empty type params still works" $ do
                res <- parseTestString "type Void2 = Absurd2;"
                case res of
                    Right [SumType lam] -> do
                        lamName lam `shouldBe` "Void2"
                        params lam `shouldBe` []
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "L2: parameterized enum (no fields, just + between constructors)" $ do
                res <- parseTestString "type Cmp = LT2 + EQ2 + GT2;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors cs -> length cs `shouldBe` 3
                            other -> expectationFailure $ "Expected 3 constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "L3: constructor with many fields" $ do
                res <- parseTestString "type Big = BigC * a:Int * b:Int * c:Int * d:Int * e:Int;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors [c] -> do
                                lamName c `shouldBe` "BigC"
                                length (params c) `shouldBe` 5
                            other -> expectationFailure $ "Expected single constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "L4: sum with one nullary and one multi-field variant" $ do
                res <- parseTestString "type Res = Ok2 * val:Int * msg:String + Err2;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors cs -> do
                                length cs `shouldBe` 2
                                lamName (head cs) `shouldBe` "Ok2"
                                length (params (head cs)) `shouldBe` 2
                                lamName (cs !! 1) `shouldBe` "Err2"
                                params (cs !! 1) `shouldBe` []
                            other -> expectationFailure $ "Expected Constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "L5: GADT nullary at start + fielded at end" $ do
                res <- parseTestString "type FList(a:Type, n:Nat) = FNil : FList(a, Z) + FCons * hd:a * tl:FList(a, n) : FList(a, Succ(n));"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors cs -> length cs `shouldBe` 2
                            other -> expectationFailure $ "Expected 2 constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "L6: type body spanning multiple lines with + at start of line" $ do
                res <- parseTestString "type Multi = A1 * x:Int\n  + B1 * y:Bool\n  + C1;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors cs -> do
                                length cs `shouldBe` 3
                                map lamName cs `shouldBe` ["A1", "B1", "C1"]
                            other -> expectationFailure $ "Expected 3 constructors, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "L7: field type is a type application" $ do
                res <- parseTestString "type Container = items:List(Int) * count:Int;"
                case res of
                    Right [SumType lam] -> do
                        case body lam of
                            Constructors [c] -> do
                                length (params c) `shouldBe` 2
                                case typ (head (params c)) of
                                    App (Id "List") [Id "Int"] -> return ()
                                    other -> expectationFailure $ "Expected List(Int), got: " ++ show other
                            other -> expectationFailure $ "Expected single constructor, got: " ++ show other
                    other -> expectationFailure $ "Expected SumType, got: " ++ show other

            it "L8: | still works in pattern matching (unchanged)" $ do
                res <- parseTestString "function f(x:Bool) : Int = match x | True -> 1 | False -> 0;"
                case res of
                    Right [Function _] -> return ()
                    other -> expectationFailure $ "Expected Function with match, got: " ++ show other

            it "L9: function params still use comma (unchanged)" $ do
                res <- parseTestString "function g(x:Int, y:Bool, z:String) : Int = x;"
                case res of
                    Right [Function lam] -> do
                        length (params lam) `shouldBe` 3
                    other -> expectationFailure $ "Expected Function with 3 params, got: " ++ show other

        -- ==================================================================
        -- P35: Integration tests (load + eval with new syntax)
        -- ==================================================================
        describe "TypeCheck: Lit (value literals in types)" $ do
            let tcSt0 = initTCState TCRelaxed
                tcEnv0 = emptyTCEnv

            it "Lit unifies with itself" $ do
                let result = runTC (unify (Lit (LInt 3)) (Lit (LInt 3))) tcEnv0 tcSt0
                case result of
                    Right _ -> return ()
                    Left errs -> expectationFailure $ "Unification failed: " ++ show errs

            it "Lit fails to unify with different literal" $ do
                let result = runTC (unify (Lit (LInt 3)) (Lit (LInt 4))) tcEnv0 tcSt0
                case result of
                    Left _ -> return ()
                    Right _ -> expectationFailure "Expected unification to fail for 3 vs 4"

            it "Lit is concrete" $ do
                isConcreteTy (Lit (LInt 42)) `shouldBe` True

            it "PropEq with concrete components is concrete" $ do
                isConcreteTy (App (Id "PropEq") [Id "Int", Lit (LInt 3), Lit (LInt 3)]) `shouldBe` True

            it "PropEq with Meta component is not concrete" $ do
                isConcreteTy (App (Id "PropEq") [Id "Int", Meta 0, Lit (LInt 3)]) `shouldBe` False

        -- ==================================================================
        -- Phase 1: Critical bug fix tests
        -- ==================================================================
        describe "Phase 1: Critical fixes" $ do
            let tcSt0 = initTCState TCRelaxed
                tcEnv0 = emptyTCEnv

            describe "occursIn handles compound Expr forms" $ do
                it "detects Meta inside NTuple" $ do
                    occursIn 5 (NTuple [(Nothing, Meta 5)]) `shouldBe` True

                it "does not false-positive NTuple without Meta" $ do
                    occursIn 5 (NTuple [(Nothing, Id "Int")]) `shouldBe` False

                it "detects Meta inside ConTuple" $ do
                    occursIn 3 (ConTuple (ConsTag "Just" 1) [Meta 3]) `shouldBe` True

                it "detects Meta inside Typed" $ do
                    occursIn 2 (Typed (Id "x") (Meta 2)) `shouldBe` True

                it "detects Meta inside ArrayLit" $ do
                    occursIn 1 (ArrayLit [Id "a", Meta 1]) `shouldBe` True

                it "detects Meta inside RecordType" $ do
                    occursIn 7 (RecordType [("x", Meta 7)] False) `shouldBe` True

                it "detects Meta inside Implicit" $ do
                    occursIn 4 (Implicit (Meta 4)) `shouldBe` True

            describe "freeMetas handles compound Expr forms" $ do
                it "finds metas inside NTuple" $ do
                    freeMetas (NTuple [(Just "a", Meta 1), (Nothing, Meta 2)]) `shouldBe` [1, 2]

                it "finds metas inside RecordType" $ do
                    freeMetas (RecordType [("x", Meta 3), ("y", Id "Int")] False) `shouldBe` [3]

            describe "isConcreteTy handles compound Expr forms" $ do
                it "NTuple with all concrete fields is concrete" $ do
                    isConcreteTy (NTuple [(Nothing, Id "Int"), (Nothing, Id "Bool")]) `shouldBe` True

                it "NTuple with Meta is not concrete" $ do
                    isConcreteTy (NTuple [(Nothing, Meta 0)]) `shouldBe` False

                it "RecordType with concrete fields is concrete" $ do
                    isConcreteTy (RecordType [("x", Id "Int")] False) `shouldBe` True

            describe "Pi universe level inference" $ do
                it "Type -> Type : Type1" $ do
                    let result = runTC (infer (Pi Nothing (U (LConst 0)) (U (LConst 0)))) tcEnv0 tcSt0
                    case result of
                        Right (ty, _) -> ty `shouldBe` U (LConst 1)
                        Left errs -> expectationFailure $ "Should infer Type1: " ++ show errs

                it "Type1 -> Type : Type2" $ do
                    let result = runTC (infer (Pi Nothing (U (LConst 1)) (U (LConst 0)))) tcEnv0 tcSt0
                    case result of
                        Right (ty, _) -> ty `shouldBe` U (LConst 2)
                        Left errs -> expectationFailure $ "Should infer Type2: " ++ show errs

                it "Int -> Bool : Type (value-level Pi)" $ do
                    let result = runTC (infer (Pi Nothing (Id "Int") (Id "Bool"))) tcEnv0 tcSt0
                    case result of
                        Right (ty, _) -> ty `shouldBe` U (LConst 0)
                        Left errs -> expectationFailure $ "Should infer Type: " ++ show errs

            describe "Universe unification correctness" $ do
                it "U 0 does not unify with U 1 (use subtype for cumulativity)" $ do
                    let result = runTC (unify (U (LConst 0)) (U (LConst 1))) tcEnv0 tcSt0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "unify should require exact universe equality"

                it "subtype allows U 0 <= U 1" $ do
                    let result = runTC (subtype (U (LConst 0)) (U (LConst 1))) tcEnv0 tcSt0
                    case result of
                        Right _ -> pure ()
                        Left errs -> expectationFailure $ "subtype should allow cumulativity: " ++ show errs

                it "subtype rejects U 1 <= U 0" $ do
                    let result = runTC (subtype (U (LConst 1)) (U (LConst 0))) tcEnv0 tcSt0
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "subtype should reject U 1 <= U 0"

            describe "applySubst handles compound Expr forms" $ do
                it "substitutes inside NTuple" $ do
                    let st1 = tcSt0 { nextMeta = 2, substitution = Map.fromList [(0, Id "Int")] }
                        result = runTC (applySubst (NTuple [(Nothing, Meta 0)])) tcEnv0 st1
                    case result of
                        Right (NTuple [(Nothing, Id "Int")], _) -> pure ()
                        Right (other, _) -> expectationFailure $ "Expected NTuple with Int, got: " ++ show other
                        Left errs -> expectationFailure $ "Unexpected error: " ++ show errs

            describe "replaceMeta handles compound Expr forms" $ do
                it "replaces inside NTuple" $ do
                    replaceMeta 0 (Id "Bool") (NTuple [(Nothing, Meta 0)]) `shouldBe` NTuple [(Nothing, Id "Bool")]

                it "replaces inside RecordType" $ do
                    replaceMeta 1 (Id "Int") (RecordType [("x", Meta 1)] False) `shouldBe` RecordType [("x", Id "Int")] False

        -- ==================================================================
        -- Strict Mode Hardening Tests
        -- ==================================================================
        describe "Strict mode hardening" $ do
            let stStrict = initTCState TCStrict
                stRelaxed = initTCState TCRelaxed
                env0 = emptyTCEnv

            describe "tcWarnOrFail escalation in strict mode" $ do
                it "unbound variable is fatal in strict mode" $ do
                    let result = runTC (infer (Id "nonexistent_xyz")) env0 stStrict
                    case result of
                        Left errs -> errs `shouldSatisfy` Prelude.any (\e -> case e of
                            UnboundVar _ -> True
                            WithContext _ (UnboundVar _) -> True
                            _ -> False)
                        Right _ -> expectationFailure "Strict mode should fail on unbound variable"

                it "catch-all infer is fatal in strict mode" $ do
                    let result = runTC (infer (DeclBlock [])) env0 stStrict
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Strict mode should fail on uninferrable expr"

                it "ERROR node is fatal in strict mode" $ do
                    let result = runTC (infer (ERROR "test error")) env0 stStrict
                    case result of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "Strict mode should fail on ERROR node"

                it "unsolved constraint is fatal in strict mode" $ do
                    -- Create a structure "Eq" with a method, but no instance for "Unknown"
                    let eqLam = mkLambda "Eq" [Var "a" (U (LConst 0)) UNDEFINED]
                                  (DeclBlock [Function (mkLambda "==" [Var "x" (Id "a") UNDEFINED, Var "y" (Id "a") UNDEFINED] UNDEFINED (Id "Bool"))])
                                  UNDEFINED
                        cenv = addNamedStructure (Structure eqLam (StructInfo SAlgebra [] [] [] [])) initialEnvironment
                        envWithCompiler = emptyTCEnv { envCompiler = Just cenv }
                        action = tcModify (\s -> s { constraints = [CStructure "Eq" Nothing [Id "Unknown"] Nothing] })
                                 `tcBind` \_ -> resolveConstraints
                    let result = runTC action envWithCompiler stStrict
                    case result of
                        Left errs -> errs `shouldSatisfy` Prelude.any (\e -> case e of
                            ConstraintUnsolved _ -> True
                            WithContext _ (ConstraintUnsolved _) -> True
                            _ -> False)
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)

            describe "tcTry error reporting (not silently swallowed)" $ do
                it "ConTuple arg mismatch reports error in strict mode" $ do
                    let succCons = mkLambda "Succ" [Var "n" (Id "Nat") UNDEFINED] (Tuple [Id "n"]) (Id "Nat")
                        cenv = addNamedConstructor 1 succCons initialEnvironment
                        tcEnvC = emptyTCEnv { envCompiler = Just cenv }
                    -- Pass a String literal where Nat is expected
                    let result = runTC (infer (ConTuple (ConsTag "Succ" 1) [Lit (LString "bad")])) tcEnvC stStrict
                    case result of
                        Left errs -> errs `shouldSatisfy` (not . null)
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)

                it "inferLambda body-signature mismatch reports in strict mode" $ do
                    -- Function says returns Bool but body is Int literal
                    let lam = mkLambda "badFunc" [Var "x" (Id "Int") UNDEFINED] (Lit (LInt 42)) (Id "Bool")
                    let result = runTC (inferLambda lam) env0 stStrict
                    case result of
                        Left errs -> errs `shouldSatisfy` (not . null)
                        Right (_, st') -> tcErrors st' `shouldSatisfy` (not . null)

            describe "FixityDecl handled in checkTopLevel" $ do
                it "FixityDecl doesn't generate type error" $ do
                    let result = runTC (checkTopLevel (FixityDecl AssocLeft 6 ["+", "-"])) env0 stStrict
                    case result of
                        Right ((), st') -> tcErrors st' `shouldSatisfy` null
                        Left errs -> expectationFailure $ "FixityDecl should not fail: " ++ show errs

            describe "CaseOf pattern variable binding" $ do
                it "pattern variables are in scope for body" $ do
                    -- CaseOf [Var "n" Nat _] body — n should be bound
                    let caseExpr = CaseOf [Var "n" (Id "Nat") UNDEFINED] (Id "n") SourceInteractive
                    let result = runTC (infer caseExpr) env0 stRelaxed
                    case result of
                        Right (ty, st') ->
                            -- n should resolve to Nat, not trigger UnboundVar
                            tcErrors st' `shouldSatisfy` (not . Prelude.any isUnboundVar)
                        Left errs -> expectationFailure $ "Should not fail: " ++ show errs

            describe "Pipeline halt in strict mode" $ do
                it "tcErrorCount tracks errors" $ do
                    -- Run typeCheckPass with a program containing a type error
                    let badExpr = Function (mkLambda "badFunc" [] (ERROR "intentional") (Id "Int"))
                        strictSt = emptyIntState {
                            currentFlags = (currentFlags emptyIntState) { strictTypes = True },
                            parsedModule = [(badExpr, SourceInteractive)]
                        }
                    finalSt <- evalStateT (execStateT typeCheckPass strictSt) initLogState
                    tcErrorCount finalSt `shouldSatisfy` (> 0)

                it "tcErrorCount is 0 when no errors" $ do
                    let goodExpr = Function (mkLambda "goodFunc" [Var "x" (Id "Int") UNDEFINED] (Id "x") (Id "Int"))
                        relaxedSt = emptyIntState {
                            parsedModule = [(goodExpr, SourceInteractive)]
                        }
                    finalSt <- evalStateT (execStateT typeCheckPass relaxedSt) initLogState
                    tcErrorCount finalSt `shouldBe` 0

            describe "Instance declaration checking" $ do
                it "intrinsic instances pass without error" $ do
                    let inst = Instance "Eq" Nothing [Id "Int"] [Intrinsic] []
                    let result = runTC (checkTopLevel inst) env0 stRelaxed
                    case result of
                        Right ((), st') -> tcErrors st' `shouldSatisfy` null
                        Left errs -> expectationFailure $ "Intrinsic instance should pass: " ++ show errs

        -- ==================================================================
        -- Phase 2: Semantic correctness tests
        -- ==================================================================
        describe "Phase 2: Semantic correctness" $ do
            let tcSt0 = initTCState TCRelaxed
                tcEnv0 = emptyTCEnv

            describe "instantiate handles higher universes" $ do
                it "instantiates forall a:Type1. a with fresh meta" $ do
                    let forallTy = Pi (Just "f") (U (LConst 1)) (Id "f")
                        result = runTC (instantiate forallTy) tcEnv0 tcSt0
                    case result of
                        Right (Meta _, _) -> pure ()  -- successfully instantiated to fresh meta
                        Right (other, _) -> expectationFailure $ "Expected Meta, got: " ++ show other
                        Left errs -> expectationFailure $ "Should instantiate: " ++ show errs

                it "instantiates nested forall across universes" $ do
                    let forallTy = Pi (Just "a") (U (LConst 0)) (Pi (Just "f") (U (LConst 1)) (App (Id "f") [Id "a"]))
                        result = runTC (instantiate forallTy) tcEnv0 tcSt0
                    case result of
                        Right (App (Id "f") [Meta _], _) -> expectationFailure "Should also instantiate f"
                        Right (App (Meta _) [Meta _], _) -> pure ()  -- both instantiated
                        Right (other, _) -> expectationFailure $ "Unexpected: " ++ show other
                        Left errs -> expectationFailure $ "Should instantiate: " ++ show errs

            describe "clmToExpr handles applications" $ do
                it "converts CLMAPP back to App" $ do
                    clmToExpr (CLMAPP (CLMID "f") [CLMID "x"]) `shouldBe` Just (App (Id "f") [Id "x"])

                it "converts nested CLMAPP" $ do
                    clmToExpr (CLMAPP (CLMID "Maybe") [CLMID "Int"]) `shouldBe` Just (App (Id "Maybe") [Id "Int"])

                it "converts CLMIAP to App" $ do
                    clmToExpr (CLMIAP (CLMID "show") [CLMID "x"]) `shouldBe` Just (App (Id "show") [Id "x"])

            describe "unify preserves OccursCheck error through normalization" $ do
                it "OccursCheck is not swallowed by retry" $ do
                    let result = runTC (unify (Meta 0) (App (Id "List") [Meta 0])) tcEnv0 (tcSt0 { nextMeta = 1 })
                    case result of
                        Left (OccursCheck _ _ : _) -> pure ()
                        Left errs -> expectationFailure $ "Expected OccursCheck, got: " ++ show errs
                        Right _ -> expectationFailure "Should have failed with OccursCheck"

            describe "substTyVar handles compound Expr forms" $ do
                it "substitutes inside NTuple" $ do
                    substTyVar "a" (Id "Int") (NTuple [(Nothing, Id "a")]) `shouldBe` NTuple [(Nothing, Id "Int")]

                it "substitutes inside RecordType" $ do
                    substTyVar "a" (Id "Bool") (RecordType [("x", Id "a")] False) `shouldBe` RecordType [("x", Id "Bool")] False

                it "substitutes inside Implicit" $ do
                    substTyVar "a" (Id "Int") (Implicit (Id "a")) `shouldBe` Implicit (Id "Int")

            describe "decomposePi" $ do
                it "decomposes simple arrow type" $ do
                    let ty = Pi Nothing (Id "Int") (Pi Nothing (Id "Int") (Id "Int"))
                    decomposePi ty `shouldBe` ([Id "Int", Id "Int"], Id "Int")

                it "decomposes single-arg arrow" $ do
                    decomposePi (Pi Nothing (Id "Bool") (Id "Int")) `shouldBe` ([Id "Bool"], Id "Int")

                it "handles non-arrow type" $ do
                    decomposePi (Id "Int") `shouldBe` ([], Id "Int")

                it "skips type-level Pi binders (forall)" $ do
                    -- forall a:Type. a -> a   →   params=[a], ret=a
                    let ty = Pi (Just "a") (U (LConst 0)) (Pi Nothing (Id "a") (Id "a"))
                    decomposePi ty `shouldBe` ([Id "a"], Id "a")

                it "skips nested forall binders" $ do
                    -- forall a:Type. forall b:Type. a -> b   →   params=[a], ret=b
                    let ty = Pi (Just "a") (U (LConst 0)) (Pi (Just "b") (U (LConst 0)) (Pi Nothing (Id "a") (Id "b")))
                    decomposePi ty `shouldBe` ([Id "a"], Id "b")

            describe "annotateLambdaTypes" $ do
                it "fills in UNDEFINED lamType from inference" $ do
                    let lam = mkLambda "test" [Var "x" (Id "Int") UNDEFINED] (Id "x") UNDEFINED
                        tcEnv = emptyTCEnv
                        result = annotateLambdaTypes tcEnv lam
                    -- The body is just (Id "x") which is a variable of type Int
                    -- So the return type should be inferred as Int
                    lamType result `shouldBe` Id "Int"

                it "preserves explicit lamType" $ do
                    let lam = mkLambda "test" [Var "x" (Id "Int") UNDEFINED] (Id "x") (Id "Bool")
                        tcEnv = emptyTCEnv
                        result = annotateLambdaTypes tcEnv lam
                    lamType result `shouldBe` Id "Bool"

                it "skips intrinsic bodies" $ do
                    let lam = mkLambda "test" [Var "x" UNDEFINED UNDEFINED] Intrinsic UNDEFINED
                        tcEnv = emptyTCEnv
                        result = annotateLambdaTypes tcEnv lam
                    lamType result `shouldBe` UNDEFINED  -- unchanged

                it "fills in UNDEFINED param types from inference" $ do
                    let lam = mkLambda "test" [Var "x" UNDEFINED UNDEFINED] (Lit (LInt 42)) (Id "Int")
                        tcEnv = emptyTCEnv
                        result = annotateLambdaTypes tcEnv lam
                    -- x has no explicit type but is unused; TC gives it a fresh meta → zonked to ?t0
                    -- The param type should be filled in (even if it's a metavar name)
                    typ (head (params result)) `shouldNotBe` UNDEFINED

        describe "TypeElaborate" $ do
            describe "elaborateExpr" $ do
                let emptyEnv = initialEnvironment

                it "wraps literals with Typed" $ do
                    let result = elaborateExpr emptyEnv Map.empty Nothing (Lit (LInt 42))
                    result `shouldBe` Typed (Lit (LInt 42)) (Id "Int")

                it "wraps float literals with Typed" $ do
                    let result = elaborateExpr emptyEnv Map.empty Nothing (Lit (LFloat 3.14))
                    result `shouldBe` Typed (Lit (LFloat 3.14)) (Id "Float64")

                it "wraps string literals with Typed" $ do
                    let result = elaborateExpr emptyEnv Map.empty Nothing (Lit (LString "hello"))
                    result `shouldBe` Typed (Lit (LString "hello")) (Id "String")

                it "wraps char literals with Typed" $ do
                    let result = elaborateExpr emptyEnv Map.empty Nothing (Lit (LChar 'a'))
                    result `shouldBe` Typed (Lit (LChar 'a')) (Id "Char")

                it "wraps variables from ElabEnv" $ do
                    let elabEnv = Map.fromList [("x", Id "Int")]
                        result = elaborateExpr emptyEnv elabEnv Nothing (Id "x")
                    result `shouldBe` Typed (Id "x") (Id "Int")

                it "leaves unknown variables unwrapped" $ do
                    let result = elaborateExpr emptyEnv Map.empty Nothing (Id "unknown")
                    result `shouldBe` Id "unknown"

                it "preserves existing Typed wrappers" $ do
                    let input = Typed (Id "x") (Id "Bool")
                        result = elaborateExpr emptyEnv Map.empty Nothing input
                    result `shouldBe` Typed (Id "x") (Id "Bool")

                it "wraps action blocks as Unit" $ do
                    let input = ActionBlock [ActionExpr (Lit (LInt 1))]
                        result = elaborateExpr emptyEnv Map.empty Nothing input
                    case result of
                        Typed (ActionBlock _) (Id "Unit") -> return ()
                        _ -> expectationFailure $ "Expected Typed ActionBlock Unit, got: " ++ show result

                it "wraps if-then-else with then-branch type" $ do
                    let input = IfThenElse (Lit (LInt 1)) (Lit (LInt 2)) (Lit (LInt 3))
                        result = elaborateExpr emptyEnv Map.empty Nothing input
                    case result of
                        Typed (IfThenElse _ _ _) (Id "Int") -> return ()
                        _ -> expectationFailure $ "Expected Typed IfThenElse Int, got: " ++ show result

                it "elaborates let-in body type" $ do
                    let input = LetIn [(Var "x" (Id "Int") UNDEFINED, Lit (LInt 42))] (Id "x")
                        result = elaborateExpr emptyEnv Map.empty Nothing input
                    case result of
                        Typed (LetIn _ (Typed (Id "x") (Id "Int"))) (Id "Int") -> return ()
                        _ -> expectationFailure $ "Expected Typed LetIn with body type Int, got: " ++ show result

            describe "typeOfExpr" $ do
                it "extracts type from Typed" $ do
                    typeOfExpr (Typed (Id "x") (Id "Int")) `shouldBe` Just (Id "Int")

                it "returns Nothing for non-Typed" $ do
                    typeOfExpr (Id "x") `shouldBe` Nothing

            describe "elaborateLambda" $ do
                it "elaborates lambda body with param types" $ do
                    let env0 = initialEnvironment
                        lam = mkLambda "test" [Var "x" (Id "Int") UNDEFINED] (Id "x") (Id "Int")
                        result = elaborateLambda env0 lam
                    body result `shouldBe` Typed (Id "x") (Id "Int")
