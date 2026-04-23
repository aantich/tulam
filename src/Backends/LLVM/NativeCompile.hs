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
import Data.List (nubBy, foldl')
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (doesFileExist)

import Surface (Name, Lambda(..), Var(..), Expr(..))
import State (Environment(..), InterpreterState(..), lookupLambda, MonomorphLevel(..), CurrentFlags(..))
import Pipeline (lambdaToCLMLambda)
import CompileDriver (CompilationPlan(..), buildCompilationPlan)
import Backends.LLVM.LIR
import CLM (CLMLam(..), CLMExpr(..))
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
    { ncOptLevel = 3
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
            let flags = currentFlags state
                plan = buildCompilationPlan (debugTrace flags) funcNames "native"
                         (monomorphLevel flags) (specLevel flags) env state
            -- Include non-intrinsic instance functions (MonoFull rewrites
            -- implicit-param calls to direct instance function references)
            let compilableInsts = Map.filter (\l -> body l /= Intrinsic) (cpInstances plan)
            let compilable = Map.union (cpFunctions plan) compilableInsts
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

                    -- Detect nullary-as-null tags using type-aware analysis.
                    -- A constructor is null-eligible only if:
                    --   1. It's nullary (no fields)
                    --   2. Its type has at least one NON-nullary constructor
                    --   3. It's the ONLY nullary constructor in its type
                    -- This prevents pure-enum types (Color3, Ordering) from
                    -- having all constructors as null (indistinguishable).
                    let globalNullary = computeNullaryAsNull env

                    -- Step 5: Lower CLM → LIR (with per-function error reporting)
                    results <- mapM (\(n, clm) -> do
                        r <- lowerFunction n clm funcTypeMap extMap globalNullary
                        case r of
                            Left (LowerError err) -> return $ Left $ LowerError $
                                "in function '" ++ n ++ "': " ++ err
                            Right ok -> return $ Right ok
                        ) (Map.toList clmFuncs)

                    case sequence results of
                        Left (LowerError err) -> return $ CompileError $
                            "LIR lowering error: " ++ err
                        Right funcsAndGlobals -> do
                            let (lirFuncLists, globalss) = unzip funcsAndGlobals
                                lirFuncs = nubBy (\a b -> lfuncName a == lfuncName b)
                                                 (concat lirFuncLists)
                                -- Generate main() if entry point specified
                                mainFunc = case ncEntryPoint config of
                                    Just ep -> [generateMain ep funcTypeMap]
                                    Nothing -> []
                                -- Only declare externs that are actually used
                                extDecls = externDeclarations extMap
                                -- Deduplicate globals by name
                                allGlobals = nubBy (\a b -> globalName a == globalName b)
                                                   (concat globalss)
                                lmod = addRuntimeExterns $ LModule "tulam_native"
                                    allGlobals
                                    (lirFuncs ++ mainFunc)
                                    extDecls
                                llvmIR = emitModule lmod

                            if ncEmitIR config
                                then return $ CompileIR llvmIR
                                else compileWithClang config llvmIR

-- | Compute nullary-as-null tags using type-aware analysis from Environment.
-- Groups constructors by their type name, then determines which (if any)
-- nullary constructors can be safely represented as null pointers.
computeNullaryAsNull :: Environment -> HSet.HashSet Name
computeNullaryAsNull env =
    let -- Group constructors by type name
        allCons = Map.toList (constructors env)
        -- Extract: (typeName, consName, tag, isNullary) for each constructor
        consInfo = [(getTypeName (lamType lam), cname, tag, null (filter (not . isImplicitVar) (params lam)))
                   | (cname, (lam, tag)) <- allCons]
        -- Group by type name
        byType = foldl' (\acc (tyName, cname, _tag, isNull) ->
                    Map.insertWith (++) tyName [(cname, isNull)] acc)
                  Map.empty consInfo
        -- For each type: find null-eligible constructor names
        nullNames = concatMap findNullCons (Map.elems byType)
    in HSet.fromList nullNames
  where
    -- Extract the base type name from a return type expression
    getTypeName (Id name) = name
    getTypeName (App (Id name) _) = name  -- parameterized: Maybe(a) → "Maybe"
    getTypeName (App f _) = getTypeName f  -- nested app
    getTypeName other = show other  -- fallback

    -- Find at most one null-eligible constructor name for a type.
    -- Null is safe when the constructor carries no payload AND the choice
    -- between null and heap-allocated forms never needs to be distinguished.
    -- Cases:
    --   (1 nullary, ≥1 non-nullary): classic sum type like Maybe. Nullary ↔ null,
    --       non-nullary ↔ heap-allocated. Pattern match distinguishes by `ptr==null`.
    --   (1 nullary, 0 non-nullary): singleton type like Unit. Only one value
    --       possible, so null is unambiguous. This eliminates per-call Unit
    --       allocations in tight mutating loops (Sieve, Queens).
    findNullCons :: [(Name, Bool)] -> [Name]
    findNullCons consList =
        let nullary = [cname | (cname, True) <- consList]
            nonNullary = [cname | (cname, False) <- consList]
        in case (nullary, nonNullary) of
            ([singleNull], _) -> [singleNull]   -- 1 nullary + any number of non-nullary
            _                 -> []               -- multiple nullary or none

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
                [ "-std=c++17", optFlag, "-flto", "-march=native"
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
        -- For void-returning functions (Unit), just call without printing
        instrs = case retTy of
            LTVoid ->
                [ ("_call", LCall llvmName [] LTVoid) ]
            LTBool ->
                [ ("result", LCall llvmName [] LTBool)
                , ("result_i32", LZext (LVar "result" LTBool) LTInt32)
                , ("_p", LCall "tlm_print_bool" [LVar "result_i32" LTInt32] LTVoid)
                , ("_n", LCall "tlm_print_newline" [] LTVoid)
                ]
            _ ->
                [ ("result", LCall llvmName [] retTy)
                , printResult retTy
                , ("_n", LCall "tlm_print_newline" [] LTVoid)
                ]
    in LFunction "main" [] LTInt32
        [ LBlock "entry" instrs (LRet (LLitInt 0 LTInt32))
        ] False []
  where
    printResult LTInt64   = ("_p", LCall "tlm_print_int" [LVar "result" LTInt64] LTVoid)
    printResult LTFloat64 = ("_p", LCall "tlm_print_float" [LVar "result" LTFloat64] LTVoid)
    printResult LTBool    = ("_p", LCall "tlm_print_bool" [LVar "result" LTInt32] LTVoid)
    printResult LTPtr     = ("_p", LCall "tlm_print_string" [LVar "result" LTPtr] LTVoid)
    printResult _         = ("_p", LCall "tlm_print_int" [LVar "result" LTInt64] LTVoid)

-- | Check if a Var has Implicit type
isImplicitVar :: Var -> Bool
isImplicitVar (Var _ (Implicit _) _) = True
isImplicitVar _ = False
