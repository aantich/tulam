{-# LANGUAGE OverloadedStrings #-}
-- | Bytecode Runner: end-to-end pipeline from tulam functions to bytecode execution.
--
-- Pipeline:
--   1. CompileDriver builds a CompilationPlan (reachability + monomorphization)
--   2. This module converts plan functions to CLM
--   3. Compiles CLM → BytecodeModule
--   4. Runs the BytecodeModule in the VM
--
-- Shares infrastructure with the LLVM native backend:
--   - CompileDriver (reachability + monomorphization)
--   - Pipeline (lambdaToCLMLambda)
--   - Target declarations from lib/Backend/LLVM/Native.tl
module Backends.Bytecode.Runner
    ( runBytecode
    , compileToBytecode
    , BytecodeResult(..)
    ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Debug.Trace (trace)

import Surface (Name, Lambda(..), hasImplicit)
import State (Environment(..), InterpreterState(..))
import Pipeline (lambdaToCLMLambda)
import CompileDriver (CompilationPlan(..), buildCompilationPlan)
import CLM (CLMLam)
import Backends.Bytecode.Compile (compileCLMModule)
import Backends.Bytecode.Module (BytecodeModule(..), dumpModule)
import Backends.Bytecode.VM (VMState, VMError, initVM, runFunctionByName)
import Backends.Bytecode.Value (Val, valToString)

-- | Result of bytecode compilation/execution.
data BytecodeResult
    = BCRunOK Val              -- ^ Execution succeeded with result
    | BCCompileError String    -- ^ Compilation failed
    | BCRuntimeError String    -- ^ VM runtime error
    | BCDisassembly String     -- ^ Disassembly output
    deriving (Show)

-- | Compile tulam functions to a BytecodeModule.
-- This is the compilation half — shared with disassembly and other tools.
compileToBytecode :: Environment -> InterpreterState -> [Name] -> Either String BytecodeModule
compileToBytecode env state funcNames =
    -- Check target declarations are loaded
    case Map.lookup "native" (targetExterns env) of
        Nothing -> Left
            "Native target not loaded. Load lib/Backend/LLVM/Native.tl first."
        Just _allTexterns -> do
            -- Step 1: Build compilation plan (reachability + monomorphization)
            let plan = buildCompilationPlan funcNames "native" env state
            let compilable = cpFunctions plan
            let compilableInsts = cpInstances plan

            if Map.null compilable
                then Left
                    "No compilable functions found. Check that the function exists and has concrete types."
                else trace ("[BC] Compilable functions: " ++ show (Map.keys compilable)
                           ++ "\n[BC] First body: " ++ case Map.elems compilable of
                                (lam:_) -> take 200 (show (body lam))
                                _ -> "<none>") $ do
                    -- Step 2: Convert plan functions to CLM
                    -- Include both top-level functions and instance functions
                    let clmFuncs = Map.map (lambdaToCLMLambda env) compilable
                    let clmInsts = Map.map (lambdaToCLMLambda env) compilableInsts
                    let allCLM = Map.union clmFuncs clmInsts

                    -- Step 3: Compile CLM → BytecodeModule
                    case compileCLMModule "tulam_bytecode" allCLM of
                        Left err -> Left $ "Bytecode compilation error: " ++ show err
                        Right bm -> Right bm

-- | Compile and run tulam functions via bytecode VM.
-- Entry point is the last function in the list (or "main" if present).
runBytecode :: Environment -> InterpreterState -> [Name] -> IO BytecodeResult
runBytecode env state funcNames = do
    case compileToBytecode env state funcNames of
        Left err -> return $ BCCompileError err
        Right bm -> do
            -- Determine entry point: prefer "main", otherwise last function
            let entryPoint = if "main" `elem` funcNames
                    then "main"
                    else last funcNames
            -- Initialize VM and run
            vm <- initVM bm
            result <- runFunctionByName vm (T.pack entryPoint) []
            case result of
                Left vmErr -> return $ BCRuntimeError (show vmErr)
                Right val  -> return $ BCRunOK val
