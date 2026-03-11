{-# LANGUAGE OverloadedStrings #-}
-- | NativeCompile: thin adapter that compiles a CompilationPlan to native binary.
--
-- Pipeline:
--   1. CompileDriver builds a CompilationPlan (reachability + monomorphization)
--   2. This module converts plan functions to CLM
--   3. Lowers CLM → LIR using ExternMap (only needed externs)
--   4. Emits LIR → LLVM IR text
--   5. Invokes clang++ with C++ runtime → native binary
module Backends.LLVM.NativeCompile
    ( compileNative
    , CompileResult(..)
    , NativeConfig(..)
    , defaultNativeConfig
    ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as HSet
import Data.Maybe (fromMaybe)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (doesFileExist)

import Surface (Name, Lambda(..), Var(..), Expr(..), hasImplicit)
import State (Environment(..), InterpreterState(..), lookupLambda)
import Pipeline (lambdaToCLMLambda)
import CompileDriver (CompilationPlan(..), buildCompilationPlan)
import Backends.LLVM.LIR
import Backends.LLVM.CLMToLIR (lowerFunction, buildExternMap,
                                surfaceTypeToLType, surfaceRetTypeToLType,
                                ExternMap, externDeclarations,
                                sanitizeName, LowerError(..))
import Backends.LLVM.LIRToLLVM (emitModule, addRuntimeExterns)

-- | Compilation result.
data CompileResult
    = CompileOK FilePath     -- ^ Path to produced binary
    | CompileIR String       -- ^ LLVM IR text (when emitIR mode)
    | CompileError String    -- ^ Error message
    deriving (Show)

-- | Configuration for native compilation.
data NativeConfig = NativeConfig
    { ncOptLevel     :: Int        -- ^ clang -O level (0-3)
    , ncEmitIR       :: Bool       -- ^ Just produce LLVM IR text, don't compile
    , ncEntryPoint   :: Maybe Name -- ^ Function to call from main()
    , ncOutputPath   :: FilePath   -- ^ Output binary path
    , ncRuntimePath  :: FilePath   -- ^ Path to runtime .cpp
    } deriving (Show)

defaultNativeConfig :: NativeConfig
defaultNativeConfig = NativeConfig
    { ncOptLevel = 1
    , ncEmitIR = False
    , ncEntryPoint = Nothing
    , ncOutputPath = "/tmp/tulam_native"
    , ncRuntimePath = "runtime/LLVM/tlm_runtime.cpp"
    }

-- | Compile tulam functions to a native binary (or LLVM IR).
compileNative :: Environment -> InterpreterState -> NativeConfig -> [Name] -> IO CompileResult
compileNative env state config funcNames = do
    -- Check target declarations are loaded
    case Map.lookup "native" (targetExterns env) of
        Nothing -> return $ CompileError
            "Native target not loaded. Load lib/Backend/LLVM/Native.tl first."
        Just allTexterns -> do
            -- Step 1: Build compilation plan (reachability + monomorphization)
            let plan = buildCompilationPlan funcNames "native" env state

            let compilable = cpFunctions plan
            if Map.null compilable
                then return $ CompileError
                    "No compilable functions found. Check that the function exists and has concrete types."
                else do
                    -- Step 2: Convert plan functions to CLM
                    let clmFuncs = Map.map (lambdaToCLMLambda env) compilable

                    -- Step 3: Build ExternMap from ONLY needed externs
                    let neededTexterns = Map.filterWithKey
                            (\k _ -> HSet.member k (cpExternRefs plan)) allTexterns
                    let extMap = buildExternMap neededTexterns

                    -- Step 4: Build function type map from Lambda signatures
                    let funcTypeMap = Map.fromList
                            [(n, ( "tulam_" ++ sanitizeName n
                                 , map (surfaceTypeToLType . typ)
                                       (filter (not . isImplicitVar) (params lam))
                                 , surfaceRetTypeToLType (lamType lam)
                                 )) | (n, lam) <- Map.toList compilable]

                    -- Step 5: Lower CLM → LIR
                    results <- mapM (\(n, clm) -> lowerFunction n clm funcTypeMap extMap)
                                    (Map.toList clmFuncs)

                    case sequence results of
                        Left (LowerError err) -> return $ CompileError $
                            "LIR lowering error: " ++ err
                        Right funcsAndGlobals -> do
                            let (lirFuncs, globalss) = unzip funcsAndGlobals
                                -- Generate main() if entry point specified
                                mainFunc = case ncEntryPoint config of
                                    Just ep -> [generateMain ep funcTypeMap]
                                    Nothing -> []
                                -- Only declare externs that are actually used
                                extDecls = externDeclarations extMap
                                lmod = addRuntimeExterns $ LModule "tulam_native"
                                    (concat globalss)
                                    (lirFuncs ++ mainFunc)
                                    extDecls

                            -- Step 6: Emit LLVM IR
                            let llvmIR = emitModule lmod

                            if ncEmitIR config
                                then return $ CompileIR llvmIR
                                else compileWithClang config llvmIR

-- | Invoke clang++ to compile LLVM IR + runtime to native binary.
compileWithClang :: NativeConfig -> String -> IO CompileResult
compileWithClang config llvmIR = do
    let llFile = ncOutputPath config ++ ".ll"
    writeFile llFile llvmIR
    hasRuntime <- doesFileExist (ncRuntimePath config)
    if not hasRuntime
        then return $ CompileError $ "Runtime not found: " ++ ncRuntimePath config
        else do
            let optFlag = "-O" ++ show (ncOptLevel config)
            (exitCode, _, stderr) <- readProcessWithExitCode "clang++"
                [ "-std=c++17", optFlag
                , llFile, ncRuntimePath config
                , "-o", ncOutputPath config
                , "-lm"
                ] ""
            case exitCode of
                ExitSuccess -> return $ CompileOK (ncOutputPath config)
                ExitFailure n -> return $ CompileError $
                    "clang++ failed (exit " ++ show n ++ "): " ++ stderr

-- | Generate a main() function that calls the entry point and prints result.
generateMain :: Name -> Map.HashMap Name (Name, [LType], LType) -> LFunction
generateMain entryName funcTypeMap =
    let (llvmName, _, retTy) = fromMaybe
            ("tulam_" ++ sanitizeName entryName, [], LTInt64)
            (Map.lookup entryName funcTypeMap)
        callInstr = ("result", LCall llvmName [] retTy)
        printInstr = case retTy of
            LTInt64   -> ("_p", LCall "tlm_print_int" [LVar "result" LTInt64] LTVoid)
            LTFloat64 -> ("_p", LCall "tlm_print_float" [LVar "result" LTFloat64] LTVoid)
            LTBool    -> ("_p", LCall "tlm_print_bool" [LVar "result" LTInt32] LTVoid)
            LTPtr     -> ("_p", LCall "tlm_print_string" [LVar "result" LTPtr] LTVoid)
            _         -> ("_p", LCall "tlm_print_int" [LVar "result" LTInt64] LTVoid)
        nlInstr = ("_n", LCall "tlm_print_newline" [] LTVoid)
    in LFunction "main" [] LTInt32
        [ LBlock "entry"
            [callInstr, printInstr, nlInstr]
            (LRet (LLitInt 0 LTInt32))
        ] False

-- | Check if a Var has Implicit type
isImplicitVar :: Var -> Bool
isImplicitVar (Var _ (Implicit _) _) = True
isImplicitVar _ = False
