-- AWFY Permute benchmark — permutation generation with mutable array
-- Ported from the SOM benchmarks (Stefan Marr)
module Main where

import Control.Monad (when)
import Control.Monad.ST
import Data.Array.ST
import Data.IORef
import Data.STRef
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)

{-# NOINLINE permuteBench #-}
permuteBench :: Int -> Int
permuteBench sz = runST $ do
  countRef <- newSTRef (0 :: Int)
  v <- newArray (0, sz - 1) (0 :: Int) :: ST s (STUArray s Int Int)
  let permute n = do
        modifySTRef' countRef (+ 1)
        when (n /= 0) $ do
          let n1 = n - 1
          permute n1
          let loop i
                | i < 0     = return ()
                | otherwise = do
                    swap v n1 i
                    permute n1
                    swap v n1 i
                    loop (i - 1)
          loop n1
  permute sz
  readSTRef countRef

swap :: STUArray s Int Int -> Int -> Int -> ST s ()
swap v i j = do
  a <- readArray v i
  b <- readArray v j
  writeArray v i b
  writeArray v j a

verify :: Int -> Bool
verify r = r == 8660

main :: IO ()
main = do
  args <- getArgs
  let numIters   = case args of { (n:_) -> read n; _ -> 10 }
      innerIters = case args of { (_:i:_) -> read i; _ -> 1 }
  -- IORef prevents GHC from treating permuteBench(6) as a compile-time constant
  szRef <- newIORef (6 :: Int)
  totalUs <- runBench numIters szRef
  putStrLn $ "Total Runtime: " ++ show totalUs ++ "us"

runBench :: Int -> IORef Int -> IO Integer
runBench numIters szRef = go numIters 0
  where
    go 0 !acc = return acc
    go n !acc = do
      t0 <- getMonotonicTimeNSec
      sz <- readIORef szRef
      let !result = permuteBench sz
      when (not (verify result)) $ putStrLn "ERROR: verification failed"
      t1 <- getMonotonicTimeNSec
      let elapsed = fromIntegral (t1 - t0) `div` 1000
      go (n - 1) (acc + elapsed)
