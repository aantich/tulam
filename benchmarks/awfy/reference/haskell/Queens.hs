-- AWFY Queens benchmark — N-queens backtracking with ST mutable arrays
-- Ported from the SOM benchmarks (Stefan Marr)
module Main where

import Control.Monad (when)
import Control.Monad.ST
import Data.Array.ST
import Data.IORef
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)

{-# NOINLINE queens #-}
queens :: Int -> Bool
queens n = runST $ do
  freeRows <- newArray (0, n - 1) True :: ST s (STUArray s Int Bool)
  freeMaxs <- newArray (0, 2 * n - 1) True :: ST s (STUArray s Int Bool)
  freeMins <- newArray (0, 2 * n - 1) True :: ST s (STUArray s Int Bool)
  queenRows <- newArray (0, n - 1) (0 :: Int) :: ST s (STUArray s Int Int)
  placeQueen freeRows freeMaxs freeMins queenRows n 0

placeQueen :: STUArray s Int Bool -> STUArray s Int Bool -> STUArray s Int Bool
           -> STUArray s Int Int -> Int -> Int -> ST s Bool
placeQueen freeRows freeMaxs freeMins queenRows n col
  | col == n  = return True
  | otherwise = tryRow 0
  where
    tryRow row
      | row == n  = return False
      | otherwise = do
          fr <- readArray freeRows row
          fm <- readArray freeMaxs (col + row)
          fn <- readArray freeMins (col - row + n - 1)
          if fr && fm && fn
            then do
              writeArray queenRows col row
              writeArray freeRows row False
              writeArray freeMaxs (col + row) False
              writeArray freeMins (col - row + n - 1) False
              ok <- placeQueen freeRows freeMaxs freeMins queenRows n (col + 1)
              if ok
                then return True
                else do
                  writeArray freeRows row True
                  writeArray freeMaxs (col + row) True
                  writeArray freeMins (col - row + n - 1) True
                  tryRow (row + 1)
            else tryRow (row + 1)

verify :: Bool -> Bool
verify = id

main :: IO ()
main = do
  args <- getArgs
  let numIters   = case args of { (n:_) -> read n; _ -> 10 }
      innerIters = case args of { (_:i:_) -> read i; _ -> 1 }
  -- IORef prevents GHC from treating queens(8) as a compile-time constant
  nRef <- newIORef (8 :: Int)
  totalUs <- runBench numIters nRef
  putStrLn $ "Total Runtime: " ++ show totalUs ++ "us"

runBench :: Int -> IORef Int -> IO Integer
runBench numIters nRef = go numIters 0
  where
    go 0 !acc = return acc
    go i !acc = do
      t0 <- getMonotonicTimeNSec
      n <- readIORef nRef
      let !result = all id (replicate 10 (queens n))
      when (not (verify result)) $ putStrLn "ERROR: verification failed"
      t1 <- getMonotonicTimeNSec
      let elapsed = fromIntegral (t1 - t0) `div` 1000
      go (i - 1) (acc + elapsed)
