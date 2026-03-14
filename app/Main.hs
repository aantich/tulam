{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.Haskeline
import System.Directory
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import System.Exit
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import State
import Control.Monad (zipWithM_, void, when, forM_)
import Data.List (intercalate, partition, sort, sortBy, isPrefixOf)

import qualified Data.Text.IO as T (readFile)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Surface
import Pipeline
import CaseOptimization (caseOptimizationPass, checkSealedExhaustiveness, terminationCheckPass)
import TypeCheck (typeCheckPass)
import CLM (pprSummary)
import Interpreter
import Logs (SourceInfo(..) )
import Util.PrettyPrinting as TC
import ModuleSystem (loadModuleTree, loadFileQuiet, baseModulePath, preludeModulePath, runModulePasses, extractDependencies, resolveModulePath, extractModulePath)
import qualified Data.Set as Set
import Interface (cacheVersion, toCachePath, ensureCacheDir)
import CLMOptimize (runCLMOptPasses, allCLMPasses, CLMOptPass(..))
import Backends.LLVM.NativeCompile (compileNative, CompileResult(..), NativeConfig(..), defaultNativeConfig)
import Backends.Bytecode.Runner (runBytecode, compileToBytecode, BytecodeResult(..))
import Backends.Bytecode.Module (dumpModule)

import Text.Pretty.Simple (pPrint, pShow)

import Data.HashMap.Strict as Map
-- Data.List already imported above

import Parser as Lambda

-- need this 4-monad stack to make sure Haskeline works with our state monad
type InputTState = InputT IntState

-- | Print only when verbosity >= given level
logAt :: Verbosity -> String -> IntState ()
logAt minLevel msg = do
    v <- gets (verbosity . currentFlags)
    when (v >= minLevel) $ liftIO $ putStrLn msg

-- | Print progress info (pass names, file loading) — requires Normal or higher
logProgress :: String -> IntState ()
logProgress = logAt Normal

-- | Print verbose/debug info — requires Verbose
logDetail :: String -> IntState ()
logDetail = logAt Verbose

processNew :: T.Text -> IntState ()
processNew line = do
    res <- parseToplevel line
    case res of
        Right ex -> do
            -- processing parsed input
            liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Received expressions: " -- ++ (show $ length ex)
            liftIO $ putStrLn (show ex) -- show what was parsed first
            traceExpr ex
            addBinding ex
            showAllLogs
            clearAllLogs
            -- processSurfaceExpr ex -- processing expressions one by one
        Left err -> do
            -- it's not a top level expression, trying interactive expression
            liftIO $ putStrLn $ "trying interactive expression"
            res1 <- parseExpr line
            case res1 of
                Left err1 -> liftIO (print err) >> liftIO (print err1)
                Right ex1 -> do
                    trace $ TC.as [TC.bold, TC.underlined] "Received interactive expression: " -- ++ (show $ length ex)
                    trace (show ex1) -- show what was parsed first
                    showAllLogs
                    clearAllLogs
                    processInteractive ex1
                    

                    


showHelp :: IO ()
showHelp = do
    putStrLn $ TC.as [TC.bold, TC.underlined] "Available commands:"
    putStrLn ":h[elp]           -- this message"
    putStrLn ":q[uit]           -- quit"
    putStrLn ":a[ll]            -- list everything that was parsed, -d - in core format"
    putStrLn ":l[oad] <name>    -- load and interpret file <name>"
    putStrLn ":s[et] <command>  -- set environment flags (:s trace on/off, :s verbose on/off, :s verbosity silent/errors/warnings/normal/verbose, :s stricttypes on/off)"
    putStrLn ":s optimize on/off      -- toggle all CLM optimization passes"
    putStrLn ":s opt:list             -- show all passes with status"
    putStrLn ":s opt:<name> on/off    -- toggle individual pass"
    putStrLn ":s opt:none             -- disable all passes"
    putStrLn ":e[nv]            -- show current environment"
    putStrLn ":clm              -- show CLM (core list machine) format functions"
    putStrLn ":clm-raw          -- show pre-optimization CLM (raw)"
    putStrLn ":inspect <name>        -- show full detail of a type, function, or constructor"
    putStrLn ":list <types, functions, constructors, targets> [-d] -- list all global functions / types / constructors / target decls"
    putStrLn ":cache status         -- show module cache stats"
    putStrLn ":cache clear          -- delete all cached modules"
    putStrLn ":cache rebuild        -- clear cache and recompile all modules"
    -- putStrLn ":i[nfo] <name>    -- find and show a top-level binding with <name>"
    -- putStrLn ":types            -- list all types"
    -- putStrLn ":compile          -- compile currently loaded program"
    -- putStrLn ":r[un] <f name>   -- execute expression with a given name that's already loaded. 'main' by detault."
        

_pprSomethingEnv sel str = do
    liftIO $ putStrLn str
    smth <- get >>= \s -> pure ( (sel . currentEnvironment) s)
    let tkeys = Map.keys smth
    liftIO $ mapM_ (fenv1 smth) tkeys
    where fenv1 ts tk = do
                        let (Just tt) = Map.lookup tk ts
                        putStrLn $ (TC.as [bold,green] (tk ++ ":")) ++ "\n  " ++ (ppr tt)

-- | Pretty-print a function signature (no body)
pprFuncSig :: String -> Lambda -> String
pprFuncSig name lam =
    "  " ++ TC.as [bold, green] name
    ++ pprSigParams (params lam) (lamType lam) (body lam)

-- | Show params and return type, handling implicit-param wrapper lambdas
pprSigParams :: [Var] -> Expr -> Expr -> String
pprSigParams outerPs (Function innerLam) _ =
    -- Implicit-param function: show [outer] (inner_params) : inner_return
    " " ++ showListSqBr ppr outerPs
    ++ showListRoBr ppr (params innerLam)
    ++ pprTyp (lamType innerLam)
pprSigParams ps retTy _ =
    -- Regular function
    (if Prelude.null ps then "" else " " ++ showListRoBr ppr ps)
    ++ pprTyp retTy

-- | Pretty-print a type definition (summary, not full body)
pprTypeDef :: String -> Expr -> String
pprTypeDef name expr = case expr of
    SumType lam ->
        "  " ++ TC.as [bold, cyan] "type " ++ TC.as [bold] (lamName lam)
        ++ pprTypeParams (params lam)
        ++ " = " ++ pprConsNames (body lam)
    Structure lam si ->
        "  " ++ TC.as [bold, yellow] (pprStructKind (structKind si) ++ " ")
        ++ TC.as [bold] (lamName lam)
        ++ pprTypeParams (params lam)
        ++ pprExtends (structExtends si)
        ++ pprRequires (structRequires si)
        ++ pprMethodNames (body lam)
    Primitive lam ->
        "  " ++ TC.as [bold, magenta] "primitive "
        ++ TC.as [bold] (lamName lam)
        ++ pprTypeParams (params lam)
        ++ pprTyp (lamType lam)
    ClassDecl lam ci ->
        "  " ++ TC.as [bold, magenta] (pprClassMod (classModifier ci) ++ "class ")
        ++ TC.as [bold] (lamName lam)
        ++ pprTypeParams (params lam)
        ++ pprClassParent (classParent ci)
        ++ pprMethodNames (body lam)
    _ -> "  " ++ TC.as [bold] name ++ pprTyp expr

-- | Show type parameters
pprTypeParams :: [Var] -> String
pprTypeParams [] = ""
pprTypeParams ps = showListRoBr ppr ps

-- | Extract just constructor names from a Constructors body
pprConsNames :: Expr -> String
pprConsNames (Constructors cs) = intercalate " | " (Prelude.map (\lam -> TC.as [bold] (lamName lam)) cs)
pprConsNames e = ppr e

-- | Extract method/function names from a DeclBlock or body
pprMethodNames :: Expr -> String
pprMethodNames (DeclBlock exprs) =
    let names = [lamName lam | Function lam <- exprs]
              ++ [lamName lam | Law lam _ <- exprs]
    in if Prelude.null names then ""
       else TC.as [dim] (" { " ++ intercalate ", " names ++ " }")
pprMethodNames _ = ""

-- | Pretty-print a constructor entry
pprConsDef :: String -> Lambda -> Int -> String
pprConsDef name lam tag =
    "  " ++ TC.as [bold, yellow] name
    ++ TC.as [dim] (" #" ++ show tag)
    ++ (if Prelude.null (params lam) then ""
        else " " ++ showListRoBr ppr (params lam))
    ++ pprTyp (lamType lam)

-- | List types with nice formatting
listTypes :: IntState ()
listTypes = do
    liftIO $ putStrLn $ "\n" ++ TC.as [bold, underlined] "Types"
    types' <- gets (types . currentEnvironment)
    let sorted = sort (Map.keys types')
    liftIO $ mapM_ (\k -> let Just v = Map.lookup k types' in putStrLn (pprTypeDef k v)) sorted
    liftIO $ putStrLn $ TC.as [dim] ("  (" ++ show (length sorted) ++ " types)")

-- | List functions with nice formatting (signature only, no body)
listFunctions :: IntState ()
listFunctions = do
    liftIO $ putStrLn $ "\n" ++ TC.as [bold, underlined] "Functions"
    lambdas' <- gets (topLambdas . currentEnvironment)
    let sorted = sort (Map.keys lambdas')
    liftIO $ mapM_ (\k -> let Just v = Map.lookup k lambdas' in putStrLn (pprFuncSig k v)) sorted
    liftIO $ putStrLn $ TC.as [dim] ("  (" ++ show (length sorted) ++ " functions)")

-- | List constructors with nice formatting
listConstructors :: IntState ()
listConstructors = do
    liftIO $ putStrLn $ "\n" ++ TC.as [bold, underlined] "Constructors"
    cons' <- gets (constructors . currentEnvironment)
    let sorted = sort (Map.keys cons')
    liftIO $ mapM_ (\k -> let Just (lam, tag) = Map.lookup k cons' in putStrLn (pprConsDef k lam tag)) sorted
    liftIO $ putStrLn $ TC.as [dim] ("  (" ++ show (length sorted) ++ " constructors)")

-- ===================== :inspect — full detail, multi-line pretty printer =====================

-- | Indented, multi-line pretty print for a Lambda
ppLambdaFull :: Int -> Lambda -> String
ppLambdaFull ind lam =
    indent ind ++ TC.as [bold, green] (lamName lam) ++ " "
    ++ showListRoBr ppr (params lam)
    ++ pprTyp (lamType lam)
    ++ ppBody ind (body lam)

-- | Pretty-print body with proper indentation
ppBody :: Int -> Expr -> String
ppBody _ UNDEFINED = ""
ppBody ind Intrinsic = " = " ++ TC.as [bold, cyan] "intrinsic"
ppBody ind Derive = " = " ++ TC.as [bold, cyan] "derive"
ppBody ind (DeclBlock exprs) = " =\n" ++ unlines' (Prelude.map (ppExprFull (ind + 2)) exprs)
ppBody ind (Constructors cs) = " =\n" ++ intercalate "\n" (Prelude.map (ppConsLamFull (ind + 2)) cs)
ppBody ind (PatternMatches ps) = " =\n" ++ indent (ind + 2) ++ "match\n"
    ++ unlines' (Prelude.map (\c -> indent (ind + 2) ++ "| " ++ ppCaseFull (ind + 4) c) ps)
ppBody ind expr = " = " ++ ppExprFull ind expr

-- | Pretty-print a constructor Lambda in a sum type
ppConsLamFull :: Int -> Lambda -> String
ppConsLamFull ind lam =
    indent ind ++ "| " ++ TC.as [bold, yellow] (lamName lam)
    ++ (if Prelude.null (params lam) then "" else showListRoBr ppr (params lam))
    ++ pprTyp (lamType lam)

-- | Full multi-line pretty-print for any Expr
ppExprFull :: Int -> Expr -> String
ppExprFull ind (Function lam) =
    indent ind ++ TC.as [bold, red] "function " ++ ppLambdaInline ind lam
ppExprFull ind (Action lam) =
    indent ind ++ TC.as [bold, blue] "action " ++ ppLambdaInline ind lam
ppExprFull ind (Value v ex) =
    indent ind ++ TC.as [bold, magenta] "value " ++ ppr v
    ++ if ex == UNDEFINED then "" else " = " ++ ppr ex
ppExprFull ind (Law lam ex) =
    indent ind ++ TC.as [bold, cyan] "law " ++ TC.as [bold] (lamName lam) ++ " "
    ++ showListRoBr ppr (params lam)
    ++ pprTyp (lamType lam)
    ++ " = " ++ ppr ex
ppExprFull ind (SumType lam) =
    indent ind ++ TC.as [bold, cyan] "type " ++ TC.as [bold] (lamName lam)
    ++ pprTypeParams (params lam)
    ++ pprTyp (lamType lam)
    ++ ppBody ind (body lam)
ppExprFull ind (Structure lam si) =
    indent ind ++ TC.as [bold, yellow] (pprStructKind (structKind si) ++ " ")
    ++ TC.as [bold] (lamName lam) ++ " "
    ++ pprTypeParams (params lam)
    ++ pprExtends (structExtends si)
    ++ pprRequires (structRequires si)
    ++ ppBody ind (body lam)
ppExprFull ind (Instance nm mTag targs impls reqs) =
    indent ind ++ TC.as [bold, green] "instance " ++ nm
    ++ showListRoBr ppr targs
    ++ maybe "" (\t -> " as " ++ t) mTag
    ++ pprRequires reqs ++ " =\n"
    ++ unlines' (Prelude.map (ppExprFull (ind + 2)) impls)
ppExprFull ind (ClassDecl lam ci) =
    indent ind ++ TC.as [bold, magenta] (pprClassMod (classModifier ci) ++ "class ")
    ++ TC.as [bold] (lamName lam) ++ " "
    ++ pprTypeParams (params lam)
    ++ pprClassParent (classParent ci)
    ++ ppBody ind (body lam)
ppExprFull ind (Primitive lam) =
    indent ind ++ TC.as [bold, magenta] "primitive "
    ++ TC.as [bold] (lamName lam)
    ++ pprTypeParams (params lam) ++ pprTyp (lamType lam)
ppExprFull ind (EffectDecl nm ps ops) =
    indent ind ++ TC.as [bold, cyan] "effect " ++ nm ++ showListRoBr ppr ps ++ " =\n"
    ++ unlines' (Prelude.map (\o -> ppExprFull (ind + 2) (Function o)) ops)
ppExprFull ind (Repr nm tp def fns _inv) =
    indent ind ++ TC.as [bold, magenta] "repr " ++ ppr nm ++ " as " ++ ppr tp
    ++ (if def then " default" else "") ++ " where\n"
    ++ unlines' (Prelude.map (ppExprFull (ind + 2)) fns)
ppExprFull ind (FixityDecl assoc prec ops) =
    indent ind ++ pprAssocLocal assoc ++ " " ++ show prec ++ " "
    ++ showListPlainSep (\o -> "(" ++ o ++ ")") ", " ops
ppExprFull ind e = indent ind ++ ppr e

-- | Lambda printed inline (for method declarations inside a body)
ppLambdaInline :: Int -> Lambda -> String
ppLambdaInline ind lam =
    TC.as [bold] (lamName lam) ++ " "
    ++ showListRoBr ppr (params lam)
    ++ pprTyp (lamType lam)
    ++ ppInlineBody ind (body lam)

-- | Body for inline lambdas — short bodies stay on same line
ppInlineBody :: Int -> Expr -> String
ppInlineBody _ UNDEFINED = ""
ppInlineBody _ Intrinsic = " = " ++ TC.as [bold, cyan] "intrinsic"
ppInlineBody _ Derive = " = " ++ TC.as [bold, cyan] "derive"
ppInlineBody ind (PatternMatches ps) = " =\n"
    ++ unlines' (Prelude.map (\c -> indent (ind + 4) ++ "| " ++ ppCaseFull (ind + 6) c) ps)
ppInlineBody ind expr
    | length (ppr expr) < 60 = " = " ++ ppr expr
    | otherwise = " =\n" ++ indent (ind + 4) ++ ppr expr

-- | Pretty-print a case branch
ppCaseFull :: Int -> Expr -> String
ppCaseFull ind (CaseOf vars body _) =
    showListPlainSep ppVarCaseOf ", " vars ++ " -> " ++ ppr body
ppCaseFull ind (ExpandedCase checks body _) =
    showListPlainSep ppr ", " checks ++ " -> " ++ ppr body
ppCaseFull ind e = ppr e

pprAssocLocal :: Assoc -> String
pprAssocLocal AssocLeft  = "infixl"
pprAssocLocal AssocRight = "infixr"
pprAssocLocal AssocNone  = "infix"

-- | Indentation helper
indent :: Int -> String
indent n = replicate n ' '

-- | Join lines without trailing newline
unlines' :: [String] -> String
unlines' = intercalate "\n"

-- | The :inspect command — look up a name in all environments and pretty-print it
inspectName :: String -> IntState ()
inspectName name = do
    env <- gets currentEnvironment
    let foundType = Map.lookup name (types env)
        foundFunc = Map.lookup name (topLambdas env)
        foundCons = Map.lookup name (constructors env)
        foundInst = findInstances name env
        foundClm  = Map.lookup name (clmLambdas env)
    let noneFound = not (isJust' foundType) && not (isJust' foundFunc)
                  && not (isJust' foundCons) && Prelude.null foundInst
                  && not (isJust' foundClm)
    if noneFound
      then liftIO $ putStrLn $ TC.as [red] ("  Not found: " ++ name)
      else do
        -- Type definition
        case foundType of
          Just expr -> liftIO $ do
            putStrLn $ TC.as [bold, underlined] "Type"
            putStrLn $ ppExprFull 2 expr
            putStrLn ""
          Nothing -> return ()
        -- Top-level function
        case foundFunc of
          Just lam -> liftIO $ do
            putStrLn $ TC.as [bold, underlined] "Function"
            putStrLn $ ppLambdaFull 2 lam
            putStrLn ""
          Nothing -> return ()
        -- Constructor
        case foundCons of
          Just (lam, tag) -> liftIO $ do
            putStrLn $ TC.as [bold, underlined] "Constructor"
            putStrLn $ indent 2 ++ TC.as [bold, yellow] name
              ++ TC.as [dim] (" (tag " ++ show tag ++ ")")
              ++ " " ++ showListRoBr ppr (params lam)
              ++ pprTyp (lamType lam)
            putStrLn ""
          Nothing -> return ()
        -- Instance lambdas containing this name
        if not (Prelude.null foundInst)
          then liftIO $ do
            putStrLn $ TC.as [bold, underlined] "Instances"
            mapM_ (\(key, lam) -> do
              putStrLn $ indent 2 ++ TC.as [bold, green] (prettyInstanceKey key)
              putStrLn $ ppLambdaFull 4 lam
              ) foundInst
            putStrLn ""
          else return ()
        -- CLM lambda
        case foundClm of
          Just clm -> liftIO $ do
            putStrLn $ TC.as [bold, underlined] "CLM"
            putStrLn $ indent 2 ++ ppr clm
            putStrLn ""
          Nothing -> return ()
  where
    isJust' Nothing = False
    isJust' (Just _) = True

-- | Find instance lambdas whose key starts with the given function name
findInstances :: String -> Environment -> [(String, Lambda)]
findInstances name env =
    [(k, lam) | (k, lam) <- Map.toList (instanceLambdas env),
                name `isPrefixOf` k,
                -- make sure it's an exact function name match (followed by \0 or end)
                length k == length name || (k !! length name) == '\0']

-- | Format instance key for display: "func\0Type1\0Type2" -> "func(Type1, Type2)"
prettyInstanceKey :: String -> String
prettyInstanceKey key = case break (== '\0') key of
    (func, [])   -> func
    (func, rest) -> func ++ "(" ++ intercalate ", " (splitOn '\0' (Prelude.tail rest)) ++ ")"
  where
    splitOn _ [] = []
    splitOn c s  = let (w, rest) = break (== c) s
                   in w : case rest of
                            []    -> []
                            (_:r) -> splitOn c r

processCommand :: [String] -> IntState ()
processCommand (":help":_) = liftIO showHelp
processCommand (":quit":_) = liftIO $ putStrLn "Goodbye." >> exitSuccess
processCommand (":load":xs) = loadFileNew (head xs)
processCommand (":set":s:xs) = processSet s xs
processCommand (":compile":"native":args) = compileNativeCmd args False
processCommand (":compile":"native-ir":args) = compileNativeCmd args True
processCommand (":compile":_) = liftIO $ putStrLn "Usage: :compile native [funcName] | :compile native-ir [funcName]"
processCommand (":bc":"run":args) = runBytecodeCmd args
processCommand (":bc":"disasm":args) = disasmBytecodeCmd args
processCommand (":bc":_) = liftIO $ putStrLn "Usage: :bc run <funcName> | :bc disasm <funcName>"
processCommand (":inspect":name:_) = inspectName name
processCommand (":inspect":_) = liftIO $ putStrLn "Usage: :inspect <name>"
processCommand (":list":"types":"-d":_) = do
    liftIO $ putStrLn "\n--------------- TYPES ----------------"
    types <- get >>= \s -> pure ( (types . currentEnvironment) s)
    liftIO $ mapM (\t -> pPrint t) types
    return ()
processCommand (":list":"functions":"-d":_) = do
    liftIO $ putStrLn "\n--------------- LAMBDAS ----------------"
    lambdas <- get >>= \s -> pure ( (topLambdas . currentEnvironment) s)
    liftIO $ mapM (\t -> pPrint t) lambdas
    return ()
processCommand (":list":"constructors":"-d":_) = do
    liftIO $ putStrLn "\n--------------- CONSTRUCTORS ----------------"
    cons <- get >>= \s -> pure ( (constructors . currentEnvironment) s)
    liftIO $ mapM (\t -> pPrint t) cons
    return ()
processCommand (":env":"-d":_) = do
    fl <- gets currentFlags
    liftIO $ print fl
    processCommand (":list":"types":"-d":[])
    processCommand (":list":"constructors":"-d":[])
    processCommand (":list":"functions":"-d":[])

processCommand (":list":"types":_) = listTypes
processCommand (":list":"functions":_) = listFunctions
processCommand (":list":"constructors":_) = listConstructors
processCommand (":list":"targets":_) = do
    env <- gets (currentEnvironment)
    let tInst = targetInstances env
        tHand = targetHandlers env
        tExtn = targetExterns env
    if Map.null tInst && Map.null tHand && Map.null tExtn
    then liftIO $ putStrLn "No target declarations loaded."
    else do
        forM_ (Map.toList tExtn) $ \(tgt, extMap) -> do
            liftIO $ putStrLn $ TC.as [bold, green] ("\nTarget: " ++ tgt ++ " — extern functions (" ++ show (Map.size extMap) ++ "):")
            forM_ (Map.toList extMap) $ \(nm, (_, _, spec)) ->
                liftIO $ putStrLn $ "  " ++ nm ++ case spec of
                    Nothing -> ""
                    Just (kind, s) -> " = " ++ kind ++ " \"" ++ s ++ "\""
        forM_ (Map.toList tInst) $ \(tgt, instMap) -> do
            liftIO $ putStrLn $ TC.as [bold, green] ("\nTarget: " ++ tgt ++ " — instances (" ++ show (Map.size instMap) ++ "):")
            forM_ (Map.keys instMap) $ \key ->
                liftIO $ putStrLn $ "  " ++ key
        forM_ (Map.toList tHand) $ \(tgt, handMap) -> do
            liftIO $ putStrLn $ TC.as [bold, green] ("\nTarget: " ++ tgt ++ " — handlers (" ++ show (Map.size handMap) ++ "):")
            forM_ (Map.toList handMap) $ \(nm, (eff, _, _)) ->
                liftIO $ putStrLn $ "  " ++ nm ++ " : " ++ eff

processCommand (":clm":"-d":_) = do
    liftIO $ putStrLn "\n--------------- CLM LAMBDAS ----------------"
    res <- get >>= \s -> pure ( (clmLambdas . currentEnvironment) s)
    let fkeys = Map.keys res
    liftIO $ mapM_ (fenv1 res) fkeys
    where fenv1 ts tk = do 
                        let (Just tt) = Map.lookup tk ts
                        putStrLn $ (TC.as [bold,green] (tk ++ ":")) ++ "\n  "
                        pPrint tt


processCommand (":clm":_) = do
    liftIO $ putStrLn "\n--------------- CLM LAMBDAS ----------------"
    res <- get >>= \s -> pure ( (clmLambdas . currentEnvironment) s)
    let fkeys = Map.keys res
    liftIO $ mapM_ (fenv1 res) fkeys
    where fenv1 ts tk = do
                        let (Just tt) = Map.lookup tk ts
                        putStrLn $ (TC.as [bold,green] (tk ++ ":"))
                        putStrLn $ ppr tt

processCommand (":clm-raw":_) = do
    liftIO $ putStrLn "\n--------------- RAW CLM LAMBDAS (pre-optimization) ----------------"
    res <- get >>= \s -> pure ( (rawCLMLambdas . currentEnvironment) s)
    if Map.null res
        then liftIO $ putStrLn "(no raw CLM snapshot — optimization may be disabled)"
        else do
            let fkeys = Map.keys res
            liftIO $ mapM_ (fenv1 res) fkeys
    where fenv1 ts tk = do
                        let (Just tt) = Map.lookup tk ts
                        putStrLn $ (TC.as [bold,green] (tk ++ ":"))
                        putStrLn $ ppr tt


processCommand (":env":_) = do
    fl <- gets currentFlags
    liftIO $ print fl
    processCommand (":list":"types":[])
    processCommand (":list":"constructors":[])
    processCommand (":list":"functions":[])
    
processCommand (":all":"-d":_) = do 
    mod <- get >>= \s -> pure (parsedModule s)
    liftIO (mapM_ (\(ex,_) -> pPrint ex ) mod )
    --processCommand ([":types"]) >> processCommand ([":functions"])
processCommand (":all":_) = do
    mod <- get >>= \s -> pure (parsedModule s)
    liftIO (mapM_ (\(ex,_) -> (putStrLn . ppr) ex ) mod )
    
   

{- 
liftIO $ putStrLn "\n--------------- JS REALM ----------------"
    jsp <- get >>= \s -> pure ( (outProgram . currentEnvironment) s)
    liftIO $ mapM_ putStrLn jsp
-}

processCommand (":cache":"status":_) = do
    let dir = ".tulam-cache/v" ++ show cacheVersion
    exists <- liftIO $ doesDirectoryExist dir
    if not exists then liftIO $ putStrLn "No cache directory found."
    else do
        files <- liftIO $ listDirectory dir
        let tliFiles = Prelude.filter (\f -> drop (Prelude.length f - 4) f == ".tli") files
        liftIO $ putStrLn $ "Cache: " ++ dir ++ "/"
        liftIO $ putStrLn $ "  " ++ show (length tliFiles) ++ " cached modules"
        st <- get
        let hashes = moduleSourceHashes st
        liftIO $ putStrLn $ "  " ++ show (Map.size hashes) ++ " source hashes tracked"

processCommand (":cache":"clear":_) = do
    let dir = ".tulam-cache"
    exists <- liftIO $ doesDirectoryExist dir
    if not exists then liftIO $ putStrLn "No cache to clear."
    else do
        liftIO $ removeDirectoryRecursive dir
        liftIO $ putStrLn "Cache cleared."

processCommand (":cache":"rebuild":_) = do
    liftIO $ putStrLn "Clearing cache and reloading all modules..."
    let dir = ".tulam-cache"
    exists <- liftIO $ doesDirectoryExist dir
    when exists $ liftIO $ removeDirectoryRecursive dir
    -- Reset state and reload from scratch
    modify (\s -> s { moduleSourceHashes = Map.empty })
    liftIO $ ensureCacheDir
    loadModuleTree baseModulePath
    liftIO $ putStrLn "Cache rebuilt."

processCommand (":cache":_) = liftIO $ do
    putStrLn ":cache status   -- show cache stats"
    putStrLn ":cache clear    -- delete all cached modules"
    putStrLn ":cache rebuild  -- clear cache and recompile all modules"

processCommand (":q":_) = processCommand [":quit"]
processCommand (":h":_) = processCommand [":help"]
processCommand (":a":"-d":_) = processCommand [":all","-d"]
processCommand (":a":_) = processCommand [":all"]
processCommand (":l":xs) = processCommand (":load":xs)
processCommand (":i":xs) = processCommand (":info":xs)
processCommand (":s":xs) = processCommand (":set":xs)
processCommand (":e":xs) = processCommand (":env":xs)

processCommand _ = liftIO $ putStrLn "Unknown command. Type :h[elp] to show available list."

-- | Compile function(s) to native binary via LLVM backend.
compileNativeCmd :: [String] -> Bool -> IntState ()
compileNativeCmd args emitIR = do
    st <- get
    let env = currentEnvironment st
    -- Check if Native.tl is loaded
    case Map.lookup "native" (targetExterns env) of
        Nothing -> liftIO $ putStrLn "Native target not loaded. Run: :load lib/Backend/LLVM/Native.tl"
        Just _ -> do
            let funcNames = args
                config = defaultNativeConfig
                    { ncEmitIR = emitIR
                    , ncEntryPoint = case funcNames of
                        [f] -> Just f
                        _   -> Nothing
                    }
            result <- liftIO $ compileNative env st config funcNames
            case result of
                CompileOK path -> liftIO $ putStrLn $ TC.as [TC.bold, TC.green] "Compiled: " ++ path
                CompileIR ir   -> liftIO $ putStr ir
                CompileError e -> liftIO $ putStrLn $ TC.as [TC.bold, TC.red] "Error: " ++ e

-- | Run function(s) via bytecode VM.
runBytecodeCmd :: [String] -> IntState ()
runBytecodeCmd [] = liftIO $ putStrLn "Usage: :bc run <funcName> [funcName2 ...]"
runBytecodeCmd args = do
    st <- get
    let env = currentEnvironment st
    result <- liftIO $ runBytecode env st args
    case result of
        BCRunOK val    -> liftIO $ putStrLn $ TC.as [TC.bold, TC.green] "Result: " ++ show val
        BCCompileError e -> liftIO $ putStrLn $ TC.as [TC.bold, TC.red] "Compile error: " ++ e
        BCRuntimeError e -> liftIO $ putStrLn $ TC.as [TC.bold, TC.red] "Runtime error: " ++ e
        BCDisassembly d  -> liftIO $ putStr d

-- | Disassemble function(s) to bytecode.
disasmBytecodeCmd :: [String] -> IntState ()
disasmBytecodeCmd [] = liftIO $ putStrLn "Usage: :bc disasm <funcName> [funcName2 ...]"
disasmBytecodeCmd args = do
    st <- get
    let env = currentEnvironment st
    case compileToBytecode env st args of
        Left err -> liftIO $ putStrLn $ TC.as [TC.bold, TC.red] "Error: " ++ err
        Right bm -> liftIO $ putStr (dumpModule bm)

-- various environment settings
-- processSet :: String -> IntState ()
processSet "strict" _ = do
    modify (\st -> st { currentFlags = (currentFlags st) { strict = True} } )
    liftIO $ putStrLn $ "Set interpretation mode to " ++ TC.as [TC.bold] "strict"

processSet "lazy" _ = do
    modify (\st -> st { currentFlags = (currentFlags st) { strict = False} } )
    liftIO $ putStrLn $ "Set interpretation mode to " ++ TC.as [TC.bold] "lazy"

processSet "pretty" _ = do
    modify (\st -> st { currentFlags = (currentFlags st) { pretty = True} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "pretty printing on"

processSet "show" _ = do
    modify (\st -> st { currentFlags = (currentFlags st) { pretty = False} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "pretty printing off"

processSet "trace" ("on":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { tracing = True} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "tracing on"
    
processSet "trace" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { tracing = False} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "tracing off"

processSet "verbose" ("on":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { verbosity = Verbose} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "verbosity verbose"

processSet "verbose" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { verbosity = Normal} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "verbosity normal"

processSet "verbosity" (level:_) = case level of
    "silent"   -> setV Silent
    "errors"   -> setV Errors
    "warnings" -> setV Warnings
    "normal"   -> setV Normal
    "verbose"  -> setV Verbose
    _          -> liftIO $ putStrLn "Usage: :s verbosity silent|errors|warnings|normal|verbose"
  where
    setV v = do
        modify (\st -> st { currentFlags = (currentFlags st) { verbosity = v } })
        liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] ("verbosity " ++ show v)

processSet "errors" ("on":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { showErrors = True } })
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "errors on"
processSet "errors" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { showErrors = False } })
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "errors off"

processSet "warnings" ("on":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { showWarnings = True } })
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "warnings on"
processSet "warnings" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { showWarnings = False } })
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "warnings off"

processSet "positivity" ("on":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { checkPositivity = True} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "positivity checking on"
processSet "positivity" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { checkPositivity = False} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "positivity checking off"

processSet "termination" ("on":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { checkTermination = True} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "termination checking on"
processSet "termination" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { checkTermination = False} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "termination checking off"

processSet "coverage" ("on":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { checkCoverage = True} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "coverage checking on"
processSet "coverage" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { checkCoverage = False} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "coverage checking off"

processSet "purity" ("on":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { checkPurity = True} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "purity checking on" ++ " (effect ops outside action blocks will warn)"
processSet "purity" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { checkPurity = False} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "purity checking off"

processSet "stricttypes" ("on":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { strictTypes = True } })
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "strict type checking on"

processSet "stricttypes" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { strictTypes = False } })
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "strict type checking off"

processSet "newstrings" ("on":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { newStrings = True} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "newstrings on" ++ " (string literals desugar to fromStringLiteral)"

processSet "newstrings" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { newStrings = False} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "newstrings off" ++ " (string literals pass through as primitive)"

processSet "optimize" ("on":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { optSettings = defaultOptFlags } })
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "optimization on (all passes)"

processSet "optimize" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { optSettings = (optSettings (currentFlags st)) { optimizeEnabled = False } } })
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "optimization off"

processSet "opt:list" _ = do
    flags <- gets (optSettings . currentFlags)
    liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Optimization passes:"
    liftIO $ putStrLn $ "  Master toggle: " ++ showOnOff (optimizeEnabled flags)
    liftIO $ mapM_ (\pass ->
        putStrLn $ "  " ++ TC.as [TC.bold] (passName pass) ++ ": "
            ++ showOnOff (isPassEnabledMain (passName pass) flags)
            ++ " — " ++ passDescr pass
        ) allCLMPasses
  where
    showOnOff True  = TC.as [TC.green] "on"
    showOnOff False = TC.as [TC.red] "off"
    isPassEnabledMain nm flags
        | not (optimizeEnabled flags) = False
        | otherwise = case nm of
            "eta-reduce"        -> optEtaReduce flags
            "constant-fold"     -> optConstantFold flags
            "known-constructor" -> optKnownConstructor flags
            "dead-code-elim"    -> optDeadCodeElim flags
            "inline-small"      -> optInlineSmall flags
            _                   -> True

processSet "opt:none" _ = do
    modify (\st -> st { currentFlags = (currentFlags st) { optSettings = OptFlags False False False False False False } })
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "all optimization passes off"

processSet s xs | take 4 s == "opt:" = do
    let passNm = drop 4 s
    case xs of
        ("on":_)  -> setOptPass passNm True
        ("off":_) -> setOptPass passNm False
        _         -> liftIO $ putStrLn $ "Usage: :s opt:" ++ passNm ++ " on/off"

processSet _ _ = liftIO $ putStrLn "Unknown :set command. Type :h[elp] to show available list."

-- | Toggle a specific optimization pass by name
setOptPass :: String -> Bool -> IntState ()
setOptPass nm val = do
    modify (\st -> st { currentFlags = (currentFlags st) {
        optSettings = updatePass nm val (optSettings (currentFlags st)) } })
    liftIO $ putStrLn $ "Set opt:" ++ TC.as [TC.bold] nm ++ " " ++ if val then "on" else "off"
  where
    updatePass "eta-reduce" v f        = f { optEtaReduce = v }
    updatePass "constant-fold" v f     = f { optConstantFold = v }
    updatePass "known-constructor" v f = f { optKnownConstructor = v }
    updatePass "dead-code-elim" v f    = f { optDeadCodeElim = v }
    updatePass "inline-small" v f      = f { optInlineSmall = v }
    updatePass _ _ f                   = f

loadFileNew :: String -> IntState ()
loadFileNew nm = do
    -- Check if module is already loaded (skip re-compilation)
    st0Pre <- get
    let modKey0 = filePathToModuleKey nm
        loaded0 = loadedModules (currentModuleEnv st0Pre)
    if Map.member modKey0 loaded0
      then logProgress $ "Module already loaded: " ++ nm
      else loadFileNewImpl nm

loadFileNewImpl :: String -> IntState ()
loadFileNewImpl nm = do
    logProgress $ "Loading file: " ++ nm
    -- Add the file's directory to search paths so imports resolve relative to it
    let fileDir = takeDirectory nm
    st0 <- get
    when (fileDir /= "" && fileDir `notElem` libSearchPaths st0) $
        modify (\s -> s { libSearchPaths = fileDir : libSearchPaths s })
    fileText <- liftIO (T.readFile nm)
    -- Save and clear parsedModule so we only parse/check THIS file's expressions
    stPre <- get
    let prevParsedModule = parsedModule stPre
    put $ stPre { parsedModule = [] }
    res <- parseWholeFile fileText nm
    st <- get
    let srcs = Map.insert nm fileText (loadedSources st)
    put $ st { currentSource = fileText, loadedSources = srcs }
    -- liftIO $ print st
    case res of
        Left err -> do
            modify (\s -> s { parsedModule = prevParsedModule })
            liftIO ( putStrLn $ "There were " ++ TC.as [TC.red] "parsing errors:") >> liftIO (putStrLn $ showSyntaxError fileText err)
        Right exprs -> do
                -- Save this module's parsedModule BEFORE loading deps
                -- (dep loading adds to parsedModule via loadFileQuiet)
                st1 <- get
                let thisModuleOnly = parsedModule st1
                let deps = extractDependencies exprs
                when (not (Prelude.null deps)) $ do
                    let searchPaths = libSearchPaths st1
                    depFiles <- liftIO $ mapM (resolveModulePath searchPaths) deps
                    let validDeps = [(d, fp) | (d, Just fp) <- zip deps depFiles]
                    forM_ validDeps $ \(depPath, depFile) -> do
                        let depKey = intercalate "." depPath
                        loaded <- gets (loadedModules . currentModuleEnv)
                        when (not (Map.member depKey loaded)) $ do
                            logProgress $ "  Loading dependency: " ++ depKey
                            loadFileQuiet depFile
                -- Restore parsedModule to ONLY this module's expressions for passes
                modify (\s -> s { parsedModule = thisModuleOnly })
                logProgress "... successfully loaded."
                logDetail $ "Received " ++ show (length thisModuleOnly) ++ " statements."
                logDetail $ "Executing pass 0: " ++ TC.as [TC.bold, TC.underlined] "after parser desugaring"
                timedPass "Pass 0 (desugar)" afterparserPass
                flushLogs
                logDetail $ "Executing pass 0.25: " ++ TC.as [TC.bold, TC.underlined] "string literal desugaring"
                timedPass "Pass 0.25 (string desugar)" stringLiteralDesugarPass
                flushLogs
                logDetail $ "Executing pass 0.5: " ++ TC.as [TC.bold, TC.underlined] "action block desugaring"
                timedPass "Pass 0.5 (action desugar)" actionDesugarPass
                flushLogs
                -- Environment dump only at Verbose level
                v <- gets (verbosity . currentFlags)
                when (v >= Verbose) $ processCommand ([":e"])
                logDetail $ "Executing pass 1: " ++ TC.as [TC.bold, TC.underlined] "initial top level environment building"
                timedPass "Pass 1 (env build)" buildEnvPass
                flushLogs
                when (v >= Verbose) $ processCommand ([":e"])
                logDetail $ "Executing pass 1.5: " ++ TC.as [TC.bold, TC.underlined] "record desugaring"
                timedPass "Pass 1.5 (record desugar)" recordDesugarPass
                flushLogs
                logDetail $ "Executing pass 2: " ++ TC.as [TC.bold, TC.underlined] "initial optimizations"
                timedPass "Pass 2 (case opt)" caseOptimizationPass
                checkSealedExhaustiveness
                flushLogs
                when (v >= Verbose) $ processCommand ([":e"])
                -- Check if all imported modules are loaded; defer TC if not
                stPreTC <- get
                let importedKeys = [intercalate "." d | d <- deps]
                    loadedKeys = loadedModules (currentModuleEnv stPreTC)
                    missingKeys = [k | k <- importedKeys, not (Map.member k loadedKeys)]
                    thisModuleParsed = parsedModule stPreTC
                if Prelude.null missingKeys
                  then do
                    logDetail $ "Executing pass 3: " ++ TC.as [TC.bold, TC.underlined] "type checking"
                    modify (\s -> s { tcErrorCount = 0 })
                    timedPass "Pass 3 (typecheck)" typeCheckPass
                    terminationCheckPass
                    flushLogs
                  else do
                    logProgress $ "  Deferring type check: imports not yet loaded: " ++ intercalate ", " missingKeys
                    modify (\s -> s { deferredTCModules =
                        (nm, thisModuleParsed, missingKeys) : deferredTCModules s })
                when (v >= Verbose) $ processCommand ([":e"])
                logDetail $ "Executing pass 4: " ++ TC.as [TC.bold, TC.underlined] "Lambdas to CLM"
                timedPass "Pass 4 (CLM)" lamToCLMPass
                flushLogs
                when (v >= Verbose) $ processCommand ([":e"])
                logDetail $ "Executing pass 4.5: " ++ TC.as [TC.bold, TC.underlined] "CLM optimization"
                timedPass "Pass 4.5 (CLM opt)" runCLMOptPasses
                flushLogs
                -- Restore accumulated parsedModule (this module's + previous)
                stAfterPasses <- get
                let allParsed = parsedModule stAfterPasses ++ prevParsedModule
                put $ stAfterPasses { parsedModule = allParsed }
                -- Register this module so subsequent loads don't re-load it
                let modPath = extractModulePath exprs
                    modKey = case modPath of
                        [] -> nm
                        path -> intercalate "." path
                stFinal <- get
                let menv = currentModuleEnv stFinal
                    newNames = allEnvNames (currentEnvironment stFinal)
                    entry = ModuleEnv modPath newNames Set.empty [] [] Map.empty
                    menv' = menv { loadedModules = Map.insert modKey entry (loadedModules menv) }
                put $ stFinal { currentModuleEnv = menv' }
                -- Run deferred TC for any modules whose deps are now all loaded
                runDeferredTC
                -- liftIO (putStrLn $ "Executing pass 4: " ++ TC.as [TC.bold, TC.underlined] "javascript code generation")
                -- compile2JSpass
                -- showAllLogsWSource
                -- clearAllLogs
                -- mod <- get >>= \s -> pure (parsedModule s)
                -- liftIO (mapM_ (\(ex,_) -> (putStrLn . show) ex ) mod )


-- | Run deferred type checking for modules whose imports are now all loaded.
-- Uses a fixpoint loop: newly registered modules may unblock other deferred modules.
runDeferredTC :: IntState ()
runDeferredTC = do
    st <- get
    let deferred = deferredTCModules st
        loaded = loadedModules (currentModuleEnv st)
        (ready, stillDeferred) = partition
            (\(_, _, missing) -> Prelude.all (`Map.member` loaded) missing)
            deferred
    put $ st { deferredTCModules = stillDeferred }
    forM_ ready $ \(modNm, modParsed, _) -> do
        logProgress $ "  Running deferred type check: " ++ modNm
        -- Temporarily set parsedModule to this module's expressions
        stBefore <- get
        let prevPM = parsedModule stBefore
        put $ stBefore { parsedModule = modParsed }
        modify (\s -> s { tcErrorCount = 0 })
        timedPass "Pass 3 (deferred typecheck)" typeCheckPass
        terminationCheckPass
        flushLogs
        -- Restore parsedModule
        modify (\s -> s { parsedModule = prevPM })
        -- Register module so other deferred modules can proceed
        registerModuleByPath modNm
    -- Fixpoint: re-check if newly registered modules unblock others
    st' <- get
    when (not (Prelude.null ready) && not (Prelude.null (deferredTCModules st'))) $
        runDeferredTC

-- | Register a module in loadedModules by its file path.
-- Extracts the module key from the file path and registers it.
registerModuleByPath :: String -> IntState ()
registerModuleByPath nm = do
    st <- get
    let modKey = filePathToModuleKey nm
        menv = currentModuleEnv st
        newNames = allEnvNames (currentEnvironment st)
        entry = ModuleEnv [] newNames Set.empty [] [] Map.empty
        menv' = menv { loadedModules = Map.insert modKey entry (loadedModules menv) }
    put $ st { currentModuleEnv = menv' }

-- | Convert a file path to a module key.
-- e.g. "lib/Reflection.tl" -> "Reflection", "lib/Instances/Nat.tl" -> "Instances.Nat"
filePathToModuleKey :: String -> String
filePathToModuleKey path =
    let stripped = stripPrefix "lib/" path
        noExt = stripSuffix ".tl" stripped
    in Prelude.map (\c -> if c == '/' then '.' else c) noExt
  where
    stripPrefix pfx s = if pfx `isPrefixOf` s then drop (length pfx) s else s
    stripSuffix sfx s =
        let rs = reverse s
            rsfx = reverse sfx
        in if rsfx `isPrefixOf` rs then reverse (drop (length sfx) rs) else s

-- Haskeline loop stacked into 3-monad stack
loop :: InputTState ()
loop = do
        minput <- getInputLine  (TC.as [TC.bold] "λtulam. ")
        case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> case input of
                [] -> loop  -- empty line, just continue
                -- if starts with ":" - then it's a command
                (':':_) -> lift (processCommand (words input)) >> loop
                -- otherwise parsing our language input
                _ -> lift (processNew $ T.pack input) >> loop

runInterpreter :: Bool -> InputTState ()
runInterpreter noStdlib = do
    if noStdlib
        then do
            liftIO $ putStrLn "Skipping standard library (--nostdlib)."
            liftIO $ putStrLn "Ready."
        else do
            liftIO $ putStrLn "Loading standard library..."
            lift $ loadModuleTree baseModulePath
            lift $ installDefaultHandlers
            liftIO $ putStrLn "Ready."
    loop

main :: IO ()
main = do
    args <- getArgs
    let noStdlib = "--nostdlib" `elem` args
    greetings
    -- setting up Haskeline loop
    -- getting to the right monad in our crazy monad stack
    initializeInterpreter >>= (runIntState (runInputT defaultSettings {historyFile=Just "./.tulam_history"} (runInterpreter noStdlib)))

greetings = do
    putStrLn "Welcome to tulam!"
    putStrLn "Version 0.0.9"
    putStrLn "(c) Copyright 2016-2023 by Anton Antich (a@s3.ag)\n"
    putStrLn "Type :help for help on commands or :load a file.\n"
    