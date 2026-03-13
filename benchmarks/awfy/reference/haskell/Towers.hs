-- AWFY Towers benchmark — Tower of Hanoi with mutable stacks
-- Ported from the SOM benchmarks (Stefan Marr)
module Main where

import Control.Monad (when)
import Control.Monad.ST
import Data.Array.ST
import Data.IORef
import Data.STRef
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)

-- Disk stack as a linked list
data Disk = DiskNone | DiskVal !Int Disk

{-# NOINLINE towers #-}
towers :: Int -> Int
towers numDisks = runST $ do
  piles <- newArray (0, 2) DiskNone :: ST s (STArray s Int Disk)
  moveCount <- newSTRef (0 :: Int)

  -- Build tower on pile 0 (largest disk first)
  let buildTower 0 = return ()
      buildTower i = do
        top <- readArray piles 0
        writeArray piles 0 (DiskVal i top)
        buildTower (i - 1)

  buildTower numDisks

  let diskSize (DiskVal s _) = s
      diskSize DiskNone      = 0

      pushDisk pile disk = do
        top <- readArray piles pile
        writeArray piles pile (DiskVal (diskSize disk) top)

      popDisk pile = do
        top <- readArray piles pile
        case top of
          DiskNone       -> error "Empty pile"
          DiskVal s rest -> do
            writeArray piles pile rest
            return (DiskVal s DiskNone)

      moveTop from to = do
        d <- popDisk from
        pushDisk to d
        modifySTRef' moveCount (+ 1)

      moveDisks n from to other
        | n == 1    = moveTop from to
        | otherwise = do
            moveDisks (n - 1) from other to
            moveTop from to
            moveDisks (n - 1) other to from

  moveDisks numDisks 0 1 2
  readSTRef moveCount

verify :: Int -> Bool
verify r = r == 8191

main :: IO ()
main = do
  args <- getArgs
  let numIters   = case args of { (n:_) -> read n; _ -> 10 }
      innerIters = case args of { (_:i:_) -> read i; _ -> 1 }
  -- IORef prevents GHC from treating towers(13) as a compile-time constant
  nRef <- newIORef (13 :: Int)
  totalUs <- runBench numIters nRef
  putStrLn $ "Total Runtime: " ++ show totalUs ++ "us"

runBench :: Int -> IORef Int -> IO Integer
runBench numIters nRef = go numIters 0
  where
    go 0 !acc = return acc
    go n !acc = do
      t0 <- getMonotonicTimeNSec
      nd <- readIORef nRef
      let !result = towers nd
      when (not (verify result)) $ putStrLn "ERROR: verification failed"
      t1 <- getMonotonicTimeNSec
      let elapsed = fromIntegral (t1 - t0) `div` 1000
      go (n - 1) (acc + elapsed)
