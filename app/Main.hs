{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.Haskeline
import System.Directory
import System.Exit
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import State
import Control.Monad (zipWithM_, void, when)

import qualified Data.Text.IO as T (readFile)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Surface
import Pipeline
import CaseOptimization (caseOptimizationPass, checkSealedExhaustiveness)
import TypeCheck (typeCheckPass)
import CLM (pprSummary)
import Interpreter
import Logs (SourceInfo(..) )
import Util.PrettyPrinting as TC
import ModuleSystem (loadModuleTree, loadFileQuiet, baseModulePath, preludeModulePath, runModulePasses)
import Interface (cacheVersion, toCachePath, ensureCacheDir)
import CLMOptimize (runCLMOptPasses, allCLMPasses, CLMOptPass(..))

import Text.Pretty.Simple (pPrint, pShow)

import Data.HashMap.Strict as Map

import Parser as Lambda

-- need this 4-monad stack to make sure Haskeline works with our state monad
type InputTState = InputT IntState

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
    putStrLn ":s[et] <command>  -- set environment flags (:s trace on/off, :s verbose on/off)"
    putStrLn ":s optimize on/off      -- toggle all CLM optimization passes"
    putStrLn ":s opt:list             -- show all passes with status"
    putStrLn ":s opt:<name> on/off    -- toggle individual pass"
    putStrLn ":s opt:none             -- disable all passes"
    putStrLn ":e[nv]            -- show current environment"
    putStrLn ":clm              -- show CLM (core list machine) format functions"
    putStrLn ":clm-raw          -- show pre-optimization CLM (raw)"
    putStrLn ":list <types, functions, constructors> [-d] -- list all global functions / types / constructors"
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
 

processCommand :: [String] -> IntState ()
processCommand (":help":_) = liftIO showHelp
processCommand (":quit":_) = liftIO $ putStrLn "Goodbye." >> exitSuccess
processCommand (":load":xs) = loadFileNew (head xs)
processCommand (":set":s:xs) = processSet s xs
processCommand (":compile":_) = compile2JSpass
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

processCommand (":list":"types":_) = 
    _pprSomethingEnv types "\n--------------- TYPES ----------------"
processCommand (":list":"functions":_) = 
    _pprSomethingEnv topLambdas "\n--------------- LAMBDAS ----------------"    
processCommand (":list":"constructors":_) = do
    liftIO $ putStrLn "\n--------------- CONSTRUCTORS ----------------"
    res <- get >>= \s -> pure ( (constructors . currentEnvironment) s)
    let fkeys = Map.keys res
    liftIO $ mapM_ (fenv1 res) fkeys
    where fenv1 ts tk = do 
                        let (Just (tt,constag)) = Map.lookup tk ts
                        putStrLn $ (TC.as [bold,green] (tk ++ "(" ++ show constag ++ "):")) ++ "\n  " ++ (ppr tt)

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
    modify (\st -> st { currentFlags = (currentFlags st) { verbose = True} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "verbose on"

processSet "verbose" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { verbose = False} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "verbose off"

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
    liftIO $ putStrLn $ "Loading file: " ++ nm
    fileText <- liftIO (T.readFile nm)
    res <- parseWholeFile fileText nm
    -- liftIO $ putStrLn $ T.unpack fileText
    -- liftIO $ print res
    st <- get
    -- liftIO $ print (newParsedModule st)
    let srcs = Map.insert nm fileText (loadedSources st)
    put $ st { currentSource = fileText, loadedSources = srcs }
    -- liftIO $ print st
    case res of
        Left err -> liftIO ( putStrLn $ "There were " ++ TC.as [TC.red] "parsing errors:") >> liftIO (putStrLn $ showSyntaxError fileText err)
        -- desugaring on the first pass
        Right exprs -> do
                -- liftIO (mapM_ (putStrLn . show) exprs) 
                liftIO (putStrLn "... successfully loaded.")
                liftIO (putStrLn $ "Received " ++ show (length (parsedModule st)) ++ " statements.")
                liftIO (putStrLn $ "Executing pass 0: " ++ TC.as [TC.bold, TC.underlined] "after parser desugaring")
                timedPass "Pass 0 (desugar)" afterparserPass
                showAllLogsWSource
                clearAllLogs
                liftIO (putStrLn $ "Executing pass 0.25: " ++ TC.as [TC.bold, TC.underlined] "string literal desugaring")
                timedPass "Pass 0.25 (string desugar)" stringLiteralDesugarPass
                showAllLogsWSource
                clearAllLogs
                liftIO (putStrLn $ "Executing pass 0.5: " ++ TC.as [TC.bold, TC.underlined] "action block desugaring")
                timedPass "Pass 0.5 (action desugar)" actionDesugarPass
                showAllLogsWSource
                clearAllLogs
                processCommand ([":e"])
                liftIO (putStrLn $ "Executing pass 1: " ++ TC.as [TC.bold, TC.underlined] "initial top level environment building")
                timedPass "Pass 1 (env build)" buildEnvPass
                showAllLogsWSource
                processCommand ([":e"])
                clearAllLogs
                liftIO (putStrLn $ "Executing pass 1.5: " ++ TC.as [TC.bold, TC.underlined] "record desugaring")
                timedPass "Pass 1.5 (record desugar)" recordDesugarPass
                showAllLogsWSource
                clearAllLogs
                liftIO (putStrLn $ "Executing pass 2: " ++ TC.as [TC.bold, TC.underlined] "initial optimizations")
                timedPass "Pass 2 (case opt)" caseOptimizationPass
                checkSealedExhaustiveness
                showAllLogsWSource
                processCommand ([":e"])
                clearAllLogs
                liftIO (putStrLn $ "Executing pass 3: " ++ TC.as [TC.bold, TC.underlined] "type checking")
                timedPass "Pass 3 (typecheck)" typeCheckPass
                showAllLogsWSource
                processCommand ([":e"])
                clearAllLogs
                liftIO (putStrLn $ "Executing pass 4: " ++ TC.as [TC.bold, TC.underlined] "Lambdas to CLM")
                timedPass "Pass 4 (CLM)" lamToCLMPass
                showAllLogsWSource
                processCommand ([":e"])
                clearAllLogs
                liftIO (putStrLn $ "Executing pass 4.5: " ++ TC.as [TC.bold, TC.underlined] "CLM optimization")
                timedPass "Pass 4.5 (CLM opt)" runCLMOptPasses
                showAllLogsWSource
                clearAllLogs
                processCommand([":h"])
                -- liftIO (putStrLn $ "Executing pass 4: " ++ TC.as [TC.bold, TC.underlined] "javascript code generation")
                -- compile2JSpass
                -- showAllLogsWSource
                -- clearAllLogs
                -- mod <- get >>= \s -> pure (parsedModule s)
                -- liftIO (mapM_ (\(ex,_) -> (putStrLn . show) ex ) mod )


-- Haskeline loop stacked into 3-monad stack
loop :: InputTState ()
loop = do
        minput <- getInputLine  (TC.as [TC.bold] "λtulam. ")
        case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> case input of
                [] -> liftIO showHelp >> loop
                -- if starts with ":" - then it's a command
                (':':_) -> lift (processCommand (words input)) >> loop
                -- otherwise parsing our language input
                _ -> lift (processNew $ T.pack input) >> loop

runInterpreter :: InputTState ()
runInterpreter = do
    liftIO $ putStrLn "Loading standard library..."
    lift $ loadModuleTree baseModulePath
    liftIO $ putStrLn "Ready."
    loop

main :: IO ()
main = do
    greetings
    -- setting up Haskeline loop
    -- getting to the right monad in our crazy monad stack
    initializeInterpreter >>= (runIntState (runInputT defaultSettings {historyFile=Just "./.tulam_history"} runInterpreter))

greetings = do
    putStrLn "Welcome to tulam!"
    putStrLn "Version 0.0.9"
    putStrLn "(c) Copyright 2016-2023 by Anton Antich (a@s3.ag)\n"
    putStrLn "Type :help for help on commands or :load a file.\n"
    