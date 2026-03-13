-- AWFY Sieve benchmark — Sieve of Eratosthenes with ST mutable array
-- Ported from the SOM benchmarks (Stefan Marr)
module Main where

import Control.Monad (when, forM_)
import Control.Monad.ST
import Data.Array.ST
import Data.IORef
import Data.STRef
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)

{-# NOINLINE sieve #-}
sieve :: Int -> Int
sieve size = runST $ do
  flags <- newArray (0, size - 1) True :: ST s (STUArray s Int Bool)
  countRef <- newSTRef (0 :: Int)
  forM_ [1 .. size - 1] $ \i -> do
    f <- readArray flags i
    when f $ do
      let prime = i + 1
      modifySTRef' countRef (+ 1)
      let mark k
            | k > size - 1 = return ()
            | otherwise    = writeArray flags k False >> mark (k + prime)
      mark (i + prime)
  readSTRef countRef

verify :: Int -> Bool
verify r = r == 669

main :: IO ()
main = do
  args <- getArgs
  let numIters   = case args of { (n:_) -> read n; _ -> 10 }
      innerIters = case args of { (_:i:_) -> read i; _ -> 1 }
  -- IORef prevents GHC from treating sieve(5000) as a compile-time constant
  szRef <- newIORef (5000 :: Int)
  totalUs <- runBench numIters szRef
  putStrLn $ "Total Runtime: " ++ show totalUs ++ "us"

runBench :: Int -> IORef Int -> IO Integer
runBench numIters szRef = go numIters 0
  where
    go 0 !acc = return acc
    go n !acc = do
      t0 <- getMonotonicTimeNSec
      sz <- readIORef szRef
      let !result = sieve sz
      when (not (verify result)) $ putStrLn "ERROR: verification failed"
      t1 <- getMonotonicTimeNSec
      let elapsed = fromIntegral (t1 - t0) `div` 1000
      go (n - 1) (acc + elapsed)
