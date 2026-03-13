-- AWFY Storage benchmark — recursive tree building with counting
-- Ported from the SOM benchmarks (Stefan Marr)
module Main where

import Control.Monad (when)
import Control.Monad.ST
import Data.IORef
import Data.STRef
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)

data Tree = Leaf !Int | Branch !Tree !Tree !Tree !Tree

-- LCG random: (seed * 1309 + 13849) .&. 65535
nextRandom :: STRef s Int -> ST s Int
nextRandom seedRef = do
  s <- readSTRef seedRef
  let s' = (s * 1309 + 13849) `mod` 65536
  writeSTRef seedRef s'
  return s'

buildTreeDepth :: Int -> STRef s Int -> STRef s Int -> ST s Tree
buildTreeDepth depth countRef seedRef = do
  modifySTRef' countRef (+ 1)
  if depth == 1
    then do
      r <- nextRandom seedRef
      return (Leaf (r `mod` 10 + 1))
    else do
      a <- buildTreeDepth (depth - 1) countRef seedRef
      b <- buildTreeDepth (depth - 1) countRef seedRef
      c <- buildTreeDepth (depth - 1) countRef seedRef
      d <- buildTreeDepth (depth - 1) countRef seedRef
      return (Branch a b c d)

-- | Sum all leaf values in the tree. Forces full tree construction.
sumLeaves :: Tree -> Int
sumLeaves (Leaf v)         = v
sumLeaves (Branch a b c d) = sumLeaves a + sumLeaves b
                            + sumLeaves c + sumLeaves d

{-# NOINLINE storageBench #-}
storageBench :: Int -> (Int, Int)
storageBench seed0 = runST $ do
  countRef <- newSTRef (0 :: Int)
  seedRef  <- newSTRef seed0
  tree <- buildTreeDepth 7 countRef seedRef
  cnt <- readSTRef countRef
  -- Return both count AND a tree-derived value so GHC can't
  -- dead-code-eliminate the tree construction.
  let !s = sumLeaves tree
  return (cnt, s)

verify :: (Int, Int) -> Bool
verify (cnt, _) = cnt == 5461

main :: IO ()
main = do
  args <- getArgs
  let numIters   = case args of { (n:_) -> read n; _ -> 10 }
      innerIters = case args of { (_:i:_) -> read i; _ -> 1 }
  -- Use IORef to pass seed so GHC can't constant-fold the benchmark call
  seedRef <- newIORef (74755 :: Int)
  totalUs <- runBench numIters seedRef
  putStrLn $ "Total Runtime: " ++ show totalUs ++ "us"

runBench :: Int -> IORef Int -> IO Integer
runBench numIters seedRef = go numIters 0
  where
    go 0 !acc = return acc
    go n !acc = do
      t0 <- getMonotonicTimeNSec
      seed <- readIORef seedRef
      let !result@(!_, !_) = storageBench seed
      when (not (verify result)) $
        putStrLn $ "ERROR: verification failed, got " ++ show result
      t1 <- getMonotonicTimeNSec
      let elapsed = fromIntegral (t1 - t0) `div` 1000
      go (n - 1) (acc + elapsed)
