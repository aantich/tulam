{-# LANGUAGE OverloadedStrings #-}
-- | Bytecode Runner: end-to-end pipeline from tulam functions to bytecode execution.
--
-- Pipeline:
--   1. CompileDriver builds a CompilationPlan (reachability + monomorphization)
--   2. This module converts plan functions to CLM
--   3. Compiles CLM → BytecodeModule
--   4. Runs the BytecodeModule in the VM
--
-- Also provides `evalInteractive` for REPL expression evaluation via bytecode.
module Backends.Bytecode.Runner
    ( runBytecode
    , evalInteractive
    , compileToBytecode
    , BytecodeResult(..)
    ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Data.List (nub)

import Surface (Name, Lambda(..), Expr(..), Var(..), hasImplicit, mkLambda)
import State (Environment(..), InterpreterState(..), MonomorphLevel(..), CurrentFlags(..))
import Pipeline (lambdaToCLMLambda)
import CompileDriver (CompilationPlan(..), buildCompilationPlan)
import CLM (CLMLam)
import Backends.Bytecode.Compile (compileCLMModule, compileCLMModuleWith)
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

-- | Evaluate an interactive REPL expression via bytecode VM.
-- Wraps the expression as a temporary nullary function, compiles, and runs it.
evalInteractive :: Environment -> InterpreterState -> Expr -> IO BytecodeResult
evalInteractive env state expr = do
    let replName = "__repl_eval__"
        -- Wrap expression as a nullary function: function __repl_eval__() = <expr>
        replLam = mkLambda replName [] expr UNDEFINED
        -- Temporarily add to topLambdas
        env' = env { topLambdas = Map.insert replName replLam (topLambdas env) }
    -- Compile and run using the augmented environment
    case compileToBytecodeWith env' state [replName] of
        Left err -> return $ BCCompileError err
        Right bm -> do
            vm <- initVM bm
            result <- runFunctionByName vm (T.pack replName) []
            case result of
                Left vmErr -> return $ BCRuntimeError (show vmErr)
                Right val  -> return $ BCRunOK val

-- | Compile tulam functions to a BytecodeModule.
-- This is the compilation half — shared with disassembly and other tools.
compileToBytecode :: Environment -> InterpreterState -> [Name] -> Either String BytecodeModule
compileToBytecode = compileToBytecodeWith

-- | Internal: compile with a given environment (allows temp function injection).
compileToBytecodeWith :: Environment -> InterpreterState -> [Name] -> Either String BytecodeModule
compileToBytecodeWith env state funcNames =
    case Map.lookup "native" (targetExterns env) of
        Nothing -> Left
            "Native target not loaded. Load lib/Backend/LLVM/Native.tl first."
        Just _allTexterns -> do
            -- Step 1: Build compilation plan (reachability + monomorphization)
            let flags = currentFlags state
                plan = buildCompilationPlan (debugTrace flags) funcNames "native"
                         (monomorphLevel flags) (specLevel flags) env state

            -- Check for unresolved implicit dispatch (fatal for compiled backends)
            case cpUnresolved plan of
                xs@(_:_) -> Left $ "Monomorphization failed — " ++ show (length xs)
                    ++ " unresolved implicit dispatch call(s):\n"
                    ++ unlines (map ("  - " ++) (nub xs))
                    ++ "Add type annotations or check that instances exist for these functions."
                [] -> do
                    let compilable = cpFunctions plan
                    let compilableInsts = cpInstances plan

                    if Map.null compilable
                        then Left
                            "No compilable functions found. Check that the function exists and has concrete types."
                        else do
                            -- Step 2: Convert plan functions to CLM
                            let clmFuncs = Map.map (lambdaToCLMLambda env) compilable
                            let clmInsts = Map.map (lambdaToCLMLambda env) compilableInsts
                            let allCLM = Map.union clmFuncs clmInsts

                            -- Step 3: Compile CLM → BytecodeModule
                            -- Build constructor tag map from environment for CLMID → NEWCON resolution
                            let consTagMap = Map.map snd (constructors env)
                            case compileCLMModuleWith "tulam_bytecode" allCLM consTagMap of
                                Left err -> Left $ "Bytecode compilation error: " ++ show err
                                Right bm -> Right bm

-- | Compile and run tulam functions via bytecode VM.
-- Entry point is the last function in the list (or "main" if present).
runBytecode :: Environment -> InterpreterState -> [Name] -> IO BytecodeResult
runBytecode env state funcNames = do
    let entryPoint = if "main" `elem` funcNames then "main" else last funcNames
    case Map.lookup entryPoint (topLambdas env) of
        Just lam | not (null (params lam)) ->
            return $ BCCompileError $
                "Invalid bytecode entry '" ++ entryPoint ++ "': expected a nullary function/action, but found arity "
                ++ show (length (params lam)) ++ "."
        _ -> case compileToBytecode env state funcNames of
            Left err -> return $ BCCompileError err
            Right bm -> do
                vm <- initVM bm
                result <- runFunctionByName vm (T.pack entryPoint) []
                case result of
                    Left vmErr -> return $ BCRuntimeError (show vmErr)
                    Right val  -> return $ BCRunOK val
