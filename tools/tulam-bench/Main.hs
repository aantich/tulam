{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock
import System.IO (hFlush, stdout)
import Text.Printf (printf)

import Surface
import CLM
import State
import Pipeline
import ModuleSystem
import Parser (parseExpr)
import CLMOptimize (runCLMOptPasses)
import Interpreter (_contEval, evalCLM)
import Logs (SourceInfo(..))
import Util.PrettyPrinting (ppr)
import Util.IOLogger (initLogState)

-- | A benchmark configuration
data Bench = Bench
    { benchName :: String
    , benchFile :: FilePath
    , benchExpr :: String
    }

-- | An optimization pass configuration
data PassConfig = PassConfig
    { pcName   :: String
    , pcFlags  :: OptFlags
    }

-- | Result of a single benchmark run
data BenchResult = BenchResult
    { brBench    :: String
    , brConfig   :: String
    , brLoadMs   :: Double    -- module load + optimization time (ms)
    , brEvalMs   :: Double    -- expression evaluation time (ms)
    , brTotalMs  :: Double    -- total time (ms)
    , brResult   :: String    -- abbreviated result
    }

--------------------------------------------------------------------------------
-- Benchmark programs
--------------------------------------------------------------------------------

benchmarks :: [Bench]
benchmarks =
    [ Bench "arith"     "tests/benchmarks/B01_Arithmetic.tl"  "benchArith()"
    , Bench "pattern"   "tests/benchmarks/B02_PatternMatch.tl" "benchPattern()"
    , Bench "listops"   "tests/benchmarks/B02_PatternMatch.tl" "benchListOps()"
    , Bench "hof"       "tests/benchmarks/B03_HigherOrder.tl"  "benchHOF()"
    , Bench "dispatch"  "tests/benchmarks/B04_Dispatch.tl"     "benchDispatch()"
    , Bench "fib20"     "tests/benchmarks/B05_Mixed.tl"        "benchFib()"
    , Bench "sort30"    "tests/benchmarks/B05_Mixed.tl"        "benchSort()"
    , Bench "tree"      "tests/benchmarks/B05_Mixed.tl"        "benchTree()"
    ]

--------------------------------------------------------------------------------
-- Pass configurations (what we want to compare)
--------------------------------------------------------------------------------

allOn, allOff :: OptFlags
allOn  = OptFlags True True True True True True
allOff = OptFlags False False False False False False

passConfigs :: [PassConfig]
passConfigs =
    [ PassConfig "no-opt"         allOff
    , PassConfig "eta-only"       allOff { optimizeEnabled = True, optEtaReduce = True }
    , PassConfig "inline-only"    allOff { optimizeEnabled = True, optInlineSmall = True }
    , PassConfig "cfold-only"     allOff { optimizeEnabled = True, optConstantFold = True }
    , PassConfig "kctor-only"     allOff { optimizeEnabled = True, optKnownConstructor = True }
    , PassConfig "dce-only"       allOff { optimizeEnabled = True, optDeadCodeElim = True }
    , PassConfig "all-opt"        allOn
    ]

--------------------------------------------------------------------------------
-- Core benchmark runner
--------------------------------------------------------------------------------

-- | Run an IntState action, returning the final InterpreterState
runIntStateS :: IntState () -> InterpreterState -> IO InterpreterState
runIntStateS act s = evalStateT (execStateT act s) initLogState

-- | Run an IntState action, returning the result value
runIntStateV :: IntState a -> InterpreterState -> IO a
runIntStateV act s = evalStateT (evalStateT act s) initLogState

-- | Load stdlib, return base state
loadBase :: IO InterpreterState
loadBase = runIntStateS setup emptyIntState
  where
    setup = do
        loadFileQuiet preludeModulePath
        let searchPaths = ["lib/"]
        allModules <- liftIO $ resolveAllDeps searchPaths Set.empty [baseModulePath]
        mapM_ (\(_, filePath) -> loadFileQuiet filePath) allModules

-- | Load a benchmark file on top of base state
loadBench :: InterpreterState -> FilePath -> IO InterpreterState
loadBench st filePath = runIntStateS (loadFileQuiet filePath) st

-- | Re-optimize with different flags (restore raw CLM, apply new flags, re-run passes)
reoptimize :: InterpreterState -> OptFlags -> IO InterpreterState
reoptimize st flags = runIntStateS reopt st
  where
    reopt = do
        s <- get
        let env = currentEnvironment s
        -- Restore pre-optimization CLM
        let env' = env { clmLambdas = rawCLMLambdas env, clmInstances = rawCLMInstances env }
        let cf = currentFlags s
        let cf' = cf { optSettings = flags }
        put s { currentEnvironment = env', currentFlags = cf' }
        -- Re-run optimization passes
        runCLMOptPasses

-- | Evaluate an expression string and return (result, time_ms)
timedEval :: InterpreterState -> String -> IO (CLMExpr, Double)
timedEval st input = do
    t0 <- getCurrentTime
    result <- runIntStateV (evalExprBench input) st
    t1 <- getCurrentTime
    let ms = realToFrac (diffUTCTime t1 t0) * 1000.0
    return (result, ms)

-- | Evaluate a tulam expression (simplified from Interpreter.hs processInteractive)
evalExprBench :: String -> IntState CLMExpr
evalExprBench input = do
    parsed <- parseExpr (T.pack input)
    case parsed of
        Left err -> return $ CLMERR ("Parse error: " ++ show err) SourceInteractive
        Right ex0 -> do
            let ex = desugarActions . afterparse $ traverseExpr afterparse ex0
            s <- get
            let env = currentEnvironment s
            let clmex = exprToCLM env ex
            ex1' <- evalCLM 0 clmex
            _contEval 1 clmex ex1'

-- | Run a single benchmark with a specific pass config
runSingle :: InterpreterState -> Bench -> PassConfig -> IO BenchResult
runSingle baseSt bench pc = do
    -- Load benchmark file
    t0 <- getCurrentTime
    st1 <- loadBench baseSt (benchFile bench)
    -- Re-optimize with this pass config
    st2 <- reoptimize st1 (pcFlags pc)
    t1 <- getCurrentTime
    let loadMs = realToFrac (diffUTCTime t1 t0) * 1000.0
    -- Evaluate the expression
    (result, evalMs) <- timedEval st2 (benchExpr bench)
    let resultStr = take 40 (ppr result)
    return BenchResult
        { brBench = benchName bench
        , brConfig = pcName pc
        , brLoadMs = loadMs
        , brEvalMs = evalMs
        , brTotalMs = loadMs + evalMs
        , brResult = resultStr
        }

--------------------------------------------------------------------------------
-- Output formatting
--------------------------------------------------------------------------------

-- | Print a nicely formatted results table
printTable :: [BenchResult] -> IO ()
printTable results = do
    putStrLn ""
    putStrLn $ replicate 100 '='
    putStrLn "  TULAM CLM OPTIMIZATION BENCHMARK RESULTS"
    putStrLn $ replicate 100 '='
    putStrLn ""

    -- Group by benchmark
    let benchNames = unique (map brBench results)
    mapM_ (printBenchGroup results) benchNames

    -- Summary: speedup of all-opt vs no-opt
    putStrLn $ replicate 100 '-'
    putStrLn "  SPEEDUP SUMMARY (all-opt vs no-opt)"
    putStrLn $ replicate 100 '-'
    printf "  %-12s  %10s  %10s  %8s\n" ("Benchmark" :: String) ("no-opt" :: String) ("all-opt" :: String) ("Speedup" :: String)
    printf "  %-12s  %10s  %10s  %8s\n" ("-----------" :: String) ("--------" :: String) ("--------" :: String) ("-------" :: String)
    mapM_ (printSpeedup results) benchNames
    putStrLn ""

printBenchGroup :: [BenchResult] -> String -> IO ()
printBenchGroup results bname = do
    let group = filter (\r -> brBench r == bname) results
    putStrLn $ "  " ++ bname ++ " (" ++ brResult (head group) ++ ")"
    printf "  %-14s  %8s  %8s  %8s\n" ("Config" :: String) ("Load" :: String) ("Eval" :: String) ("Total" :: String)
    printf "  %-14s  %8s  %8s  %8s\n" ("--------------" :: String) ("------" :: String) ("------" :: String) ("------" :: String)
    mapM_ (\r -> printf "  %-14s  %7.1fms  %7.1fms  %7.1fms\n"
        (brConfig r) (brLoadMs r) (brEvalMs r) (brTotalMs r)) group
    putStrLn ""

printSpeedup :: [BenchResult] -> String -> IO ()
printSpeedup results bname = do
    let noOpt  = brEvalMs <$> findResult results bname "no-opt"
    let allOpt = brEvalMs <$> findResult results bname "all-opt"
    case (noOpt, allOpt) of
        (Just n, Just a) | a > 0 ->
            printf "  %-12s  %8.1fms  %8.1fms  %7.2fx\n" bname n a (n / a)
        _ -> printf "  %-12s  %10s  %10s  %8s\n" bname ("N/A" :: String) ("N/A" :: String) ("N/A" :: String)

findResult :: [BenchResult] -> String -> String -> Maybe BenchResult
findResult rs bname cname = case filter (\r -> brBench r == bname && brConfig r == cname) rs of
    (r:_) -> Just r
    []    -> Nothing

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Loading standard library..."
    hFlush stdout
    baseSt <- loadBase
    putStrLn "Running benchmarks..."
    putStrLn ""

    results <- sequence
        [ do putStr $ "  " ++ benchName b ++ " / " ++ pcName pc ++ "..."
             hFlush stdout
             r <- runSingle baseSt b pc
             printf " %.1fms\n" (brEvalMs r)
             hFlush stdout
             return r
        | b <- benchmarks
        , pc <- passConfigs
        ]

    printTable results
