-- AWFY Bounce benchmark — 100 balls bouncing in a 500×500 box
-- Ported from the SOM benchmarks (Stefan Marr)
module Main where

import Control.Monad (when, forM_)
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)

-- LCG random: (seed * 1309 + 13849) .&. 65535
nextRandom :: STRef s Int -> ST s Int
nextRandom seedRef = do
  s <- readSTRef seedRef
  let s' = (s * 1309 + 13849) `mod` 65536
  writeSTRef seedRef s'
  return s'

-- Ball state: flat array, 4 ints per ball (x, y, xVel, yVel)
bounce :: Int -> Int
bounce ballCount = runST $ do
  seed <- newSTRef (74755 :: Int)
  balls <- newArray_ (0, ballCount * 4 - 1) :: ST s (STUArray s Int Int)

  -- Initialize balls
  forM_ [0 .. ballCount - 1] $ \i -> do
    x    <- nextRandom seed
    y    <- nextRandom seed
    xVel <- nextRandom seed
    yVel <- nextRandom seed
    writeArray balls (i * 4 + 0) (x `mod` 500)
    writeArray balls (i * 4 + 1) (y `mod` 500)
    writeArray balls (i * 4 + 2) (xVel `mod` 300 - 150)
    writeArray balls (i * 4 + 3) (yVel `mod` 300 - 150)

  bouncesRef <- newSTRef (0 :: Int)

  -- Run 50 timesteps
  forM_ [1 .. 50 :: Int] $ \_ -> do
    forM_ [0 .. ballCount - 1] $ \j -> do
      x    <- readArray balls (j * 4 + 0)
      y    <- readArray balls (j * 4 + 1)
      xVel <- readArray balls (j * 4 + 2)
      yVel <- readArray balls (j * 4 + 3)
      let x'  = x + xVel
          y'  = y + yVel
      -- Check wall bounces and update
      let (x1, xv1, b1) = bounceWall x' xVel 500
          (y1, yv1, b2) = bounceWall y' yVel 500
      writeArray balls (j * 4 + 0) x1
      writeArray balls (j * 4 + 1) y1
      writeArray balls (j * 4 + 2) xv1
      writeArray balls (j * 4 + 3) yv1
      when (b1 || b2) $ modifySTRef' bouncesRef (+ 1)

  readSTRef bouncesRef

bounceWall :: Int -> Int -> Int -> (Int, Int, Bool)
bounceWall pos vel limit =
  let (p1, v1, b1) = if pos > limit then (limit, negate (abs vel), True) else (pos, vel, False)
      (p2, v2, b2) = if p1 < 0 then (0, abs v1, True) else (p1, v1, False)
  in (p2, v2, b1 || b2)

benchmark :: Int -> Int
benchmark _ = bounce 100

verify :: Int -> Bool
verify r = r == 1331

main :: IO ()
main = do
  args <- getArgs
  let numIters   = case args of { (n:_) -> read n; _ -> 10 }
      innerIters = case args of { (_:i:_) -> read i; _ -> 1 }
  totalUs <- runBench numIters innerIters
  putStrLn $ "Total Runtime: " ++ show totalUs ++ "us"

runBench :: Int -> Int -> IO Integer
runBench numIters inner = go numIters 0
  where
    go 0 !acc = return acc
    go n !acc = do
      t0 <- getMonotonicTimeNSec
      let !result = benchmark inner
      when (not (verify result)) $
        putStrLn $ "ERROR: verification failed, got " ++ show result
      t1 <- getMonotonicTimeNSec
      let elapsed = fromIntegral (t1 - t0) `div` 1000
      go (n - 1) (acc + elapsed)
