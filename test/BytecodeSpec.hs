{-# LANGUAGE OverloadedStrings #-}
module BytecodeSpec (bytecodeTests) where

import Test.Hspec
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V

import Surface (ConsTag(..), Literal(..))
import CLM
import Backends.Bytecode.Value
import Backends.Bytecode.Instruction
import Backends.Bytecode.Module
import Backends.Bytecode.Compile
import Backends.Bytecode.VM
import Backends.Bytecode.Debug

-- | All bytecode VM tests.
bytecodeTests :: Spec
bytecodeTests = describe "Bytecode VM" $ do
    instructionTests
    compileTests
    vmTests

-- | Instruction encoding/decoding tests.
instructionTests :: Spec
instructionTests = describe "Instruction encoding" $ do
    it "encodes and decodes LOADINT" $ do
        let instr = ILoadInt 3 42
            w = encodeInstr instr
        decodeInstr w `shouldBe` Just instr

    it "encodes and decodes ADDI" $ do
        let instr = IAddI 0 1 2
            w = encodeInstr instr
        decodeInstr w `shouldBe` Just instr

    it "encodes and decodes RET" $ do
        let instr = IRet 5
            w = encodeInstr instr
        decodeInstr w `shouldBe` Just instr

    it "encodes and decodes JMP with negative offset" $ do
        let instr = IJmp (-10)
            w = encodeInstr instr
        decodeInstr w `shouldBe` Just instr

    it "encodes and decodes CALL" $ do
        let instr = ICall 0 1 3
            w = encodeInstr instr
        decodeInstr w `shouldBe` Just instr

    it "encodes and decodes NEWCON" $ do
        let instr = INewCon 0 2 3
            w = encodeInstr instr
        decodeInstr w `shouldBe` Just instr

    it "encodes and decodes GETFIELD" $ do
        let instr = IGetField 0 1 2
            w = encodeInstr instr
        decodeInstr w `shouldBe` Just instr

    it "roundtrips all basic instructions" $ do
        let instrs = [ ILoadTrue 0, ILoadFalse 1, ILoadUnit 2, ILoadNil 3
                      , IMov 0 1, IHalt, INop
                      , ISubI 0 1 2, IMulI 0 1 2
                      , IEqI 0 1 2, ILtI 0 1 2
                      , IGtI 0 1 2, IGeI 0 1 2
                      ]
        mapM_ (\i -> decodeInstr (encodeInstr i) `shouldBe` Just i) instrs

-- | Compiler tests.
compileTests :: Spec
compileTests = describe "CLM to bytecode compiler" $ do
    it "compiles a simple integer literal function" $ do
        -- function f() = 42
        let lam = CLMLam [] (CLMLIT (LInt 42))
            funcs = Map.fromList [("f", lam)]
        case compileCLMModule "test" funcs of
            Left err -> expectationFailure $ "Compilation failed: " ++ show err
            Right bm -> do
                V.length (bmFunctions bm) `shouldBe` 1
                fiName (bmFunctions bm V.! 0) `shouldBe` "f"
                fiArity (bmFunctions bm V.! 0) `shouldBe` 0

    it "compiles a function with parameters" $ do
        -- function add(x, y) = x + y  (represented as CLMIAP)
        let lam = CLMLam [("x", CLMEMPTY), ("y", CLMEMPTY)]
                    (CLMLIT (LInt 0))  -- simplified
            funcs = Map.fromList [("add", lam)]
        case compileCLMModule "test" funcs of
            Left err -> expectationFailure $ "Compilation failed: " ++ show err
            Right bm -> do
                V.length (bmFunctions bm) `shouldBe` 1
                fiArity (bmFunctions bm V.! 0) `shouldBe` 2

    it "compiles constructor creation" $ do
        -- function mkPair() = Pair(1, 2)
        let lam = CLMLam [] (CLMCON (ConsTag "Pair" 0) [CLMLIT (LInt 1), CLMLIT (LInt 2)])
            funcs = Map.fromList [("mkPair", lam)]
        case compileCLMModule "test" funcs of
            Left err -> expectationFailure $ "Compilation failed: " ++ show err
            Right bm -> do
                V.length (bmFunctions bm) `shouldBe` 1

-- | VM execution tests.
vmTests :: Spec
vmTests = describe "VM execution" $ do
    it "returns an integer literal" $ do
        let lam = CLMLam [] (CLMLIT (LInt 42))
            funcs = Map.fromList [("f", lam)]
        case compileCLMModule "test" funcs of
            Left err -> expectationFailure $ "Compilation failed: " ++ show err
            Right bm -> do
                vm <- initVM bm
                result <- runFunction vm 0 []
                result `shouldBe` Right (VInt 42)

    it "returns a boolean true" $ do
        let lam = CLMLam [] (CLMCON (ConsTag "True" 0) [])
            funcs = Map.fromList [("f", lam)]
        case compileCLMModule "test" funcs of
            Left err -> expectationFailure $ "Compilation failed: " ++ show err
            Right bm -> do
                vm <- initVM bm
                result <- runFunction vm 0 []
                case result of
                    Right (VBool True) -> return ()
                    other -> expectationFailure $ "Expected VBool True, got: " ++ show other

    it "constructs and accesses fields" $ do
        -- function f() = let p = Pair(10, 20) in p.0
        let lam = CLMLam []
                    (CLMPROG
                        [ CLMBIND "p" (CLMCON (ConsTag "Pair" 0) [CLMLIT (LInt 10), CLMLIT (LInt 20)])
                        , CLMFieldAccess ("", 0) (CLMID "p")
                        ])
            funcs = Map.fromList [("f", lam)]
        case compileCLMModule "test" funcs of
            Left err -> expectationFailure $ "Compilation failed: " ++ show err
            Right bm -> do
                vm <- initVM bm
                result <- runFunction vm 0 []
                result `shouldBe` Right (VInt 10)

    it "calls another function" $ do
        -- function double(x) = x + x  (simplified: just return x for now)
        -- function main() = double(21)
        let doubleLam = CLMLam [("x", CLMEMPTY)] (CLMID "x")
            mainLam = CLMLam [] (CLMAPP (CLMID "double") [CLMLIT (LInt 21)])
            funcs = Map.fromList [("double", doubleLam), ("main", mainLam)]
        case compileCLMModule "test" funcs of
            Left err -> expectationFailure $ "Compilation failed: " ++ show err
            Right bm -> do
                vm <- initVM bm
                case lookupFunctionByName bm "main" of
                    Nothing -> expectationFailure "main not found"
                    Just (idx, _) -> do
                        result <- runFunction vm idx []
                        result `shouldBe` Right (VInt 21)

    it "handles sequential expressions (CLMPROG)" $ do
        let lam = CLMLam []
                    (CLMPROG [CLMLIT (LInt 1), CLMLIT (LInt 2), CLMLIT (LInt 3)])
            funcs = Map.fromList [("f", lam)]
        case compileCLMModule "test" funcs of
            Left err -> expectationFailure $ "Compilation failed: " ++ show err
            Right bm -> do
                vm <- initVM bm
                result <- runFunction vm 0 []
                result `shouldBe` Right (VInt 3)

    it "returns Unit for empty" $ do
        let lam = CLMLam [] CLMEMPTY
            funcs = Map.fromList [("f", lam)]
        case compileCLMModule "test" funcs of
            Left err -> expectationFailure $ "Compilation failed: " ++ show err
            Right bm -> do
                vm <- initVM bm
                result <- runFunction vm 0 []
                result `shouldBe` Right VUnit

    it "field access on second field" $ do
        let lam = CLMLam []
                    (CLMPROG
                        [ CLMBIND "p" (CLMCON (ConsTag "Pair" 0) [CLMLIT (LInt 10), CLMLIT (LInt 20)])
                        , CLMFieldAccess ("", 1) (CLMID "p")
                        ])
            funcs = Map.fromList [("f", lam)]
        case compileCLMModule "test" funcs of
            Left err -> expectationFailure $ "Compilation failed: " ++ show err
            Right bm -> do
                vm <- initVM bm
                result <- runFunction vm 0 []
                result `shouldBe` Right (VInt 20)

    it "disassembles a function" $ do
        let lam = CLMLam [] (CLMLIT (LInt 42))
            funcs = Map.fromList [("f", lam)]
        case compileCLMModule "test" funcs of
            Left err -> expectationFailure $ "Compilation failed: " ++ show err
            Right bm -> do
                let output = disassembleModule bm
                output `shouldContain` "f"
                output `shouldContain` "LOADINT"
