-- AWFY NBody benchmark — gravitational N-body simulation
-- Ported from the SOM benchmarks (Stefan Marr)
module Main where

import Control.Monad (when, forM_)
import Data.Array.IO
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)

-- Each body has 7 fields: x, y, z, vx, vy, vz, mass
-- 5 bodies × 7 fields = 35 elements in a flat array
numBodies :: Int
numBodies = 5

bodySize :: Int
bodySize = 7

bX, bY, bZ, bVX, bVY, bVZ, bMass :: Int
bX = 0; bY = 1; bZ = 2; bVX = 3; bVY = 4; bVZ = 5; bMass = 6

solarMass :: Double
solarMass = 4 * pi * pi

daysPerYear :: Double
daysPerYear = 365.24

createSystem :: IO (IOUArray Int Double)
createSystem = do
  arr <- newArray (0, numBodies * bodySize - 1) 0.0
  -- Sun (index 0)
  writeArray arr (0 * bodySize + bMass) solarMass
  -- Jupiter (index 1)
  writeArray arr (1 * bodySize + bX)    4.84143144246472090e+00
  writeArray arr (1 * bodySize + bY)    (-1.16032004402742839e+00)
  writeArray arr (1 * bodySize + bZ)    (-1.03622044471123109e-01)
  writeArray arr (1 * bodySize + bVX)   (1.66007664274403694e-03 * daysPerYear)
  writeArray arr (1 * bodySize + bVY)   (7.69901118419740425e-03 * daysPerYear)
  writeArray arr (1 * bodySize + bVZ)   ((-6.90460016972063023e-05) * daysPerYear)
  writeArray arr (1 * bodySize + bMass) (9.54791938424326609e-04 * solarMass)
  -- Saturn (index 2)
  writeArray arr (2 * bodySize + bX)    8.34336671824457987e+00
  writeArray arr (2 * bodySize + bY)    4.12479856412430479e+00
  writeArray arr (2 * bodySize + bZ)    (-4.03523417114321381e-01)
  writeArray arr (2 * bodySize + bVX)   ((-2.76742510726862411e-03) * daysPerYear)
  writeArray arr (2 * bodySize + bVY)   (4.99852801234917238e-03 * daysPerYear)
  writeArray arr (2 * bodySize + bVZ)   (2.30417297573763929e-05 * daysPerYear)
  writeArray arr (2 * bodySize + bMass) (2.85885980666130812e-04 * solarMass)
  -- Uranus (index 3)
  writeArray arr (3 * bodySize + bX)    1.28943695621391310e+01
  writeArray arr (3 * bodySize + bY)    (-1.51111514016986312e+01)
  writeArray arr (3 * bodySize + bZ)    (-2.23307578892655734e-01)
  writeArray arr (3 * bodySize + bVX)   (2.96460137564761618e-03 * daysPerYear)
  writeArray arr (3 * bodySize + bVY)   (2.37847173959480950e-03 * daysPerYear)
  writeArray arr (3 * bodySize + bVZ)   ((-2.96589568540237556e-05) * daysPerYear)
  writeArray arr (3 * bodySize + bMass) (4.36624404335156298e-05 * solarMass)
  -- Neptune (index 4)
  writeArray arr (4 * bodySize + bX)    1.53796971148509165e+01
  writeArray arr (4 * bodySize + bY)    (-2.59193146099879641e+01)
  writeArray arr (4 * bodySize + bZ)    1.79258772950371181e-01
  writeArray arr (4 * bodySize + bVX)   (2.68067772490389322e-03 * daysPerYear)
  writeArray arr (4 * bodySize + bVY)   (1.62824170038242295e-03 * daysPerYear)
  writeArray arr (4 * bodySize + bVZ)   ((-9.51592254519715870e-05) * daysPerYear)
  writeArray arr (4 * bodySize + bMass) (5.15138902046611451e-05 * solarMass)
  -- Offset momentum
  offsetMomentum arr
  return arr

offsetMomentum :: IOUArray Int Double -> IO ()
offsetMomentum arr = do
  let go i px py pz
        | i >= numBodies = do
            writeArray arr (0 * bodySize + bVX) (negate px / solarMass)
            writeArray arr (0 * bodySize + bVY) (negate py / solarMass)
            writeArray arr (0 * bodySize + bVZ) (negate pz / solarMass)
        | otherwise = do
            vx <- readArray arr (i * bodySize + bVX)
            vy <- readArray arr (i * bodySize + bVY)
            vz <- readArray arr (i * bodySize + bVZ)
            m  <- readArray arr (i * bodySize + bMass)
            go (i + 1) (px + vx * m) (py + vy * m) (pz + vz * m)
  go 0 0 0 0

advance :: IOUArray Int Double -> Double -> IO ()
advance arr dt = do
  -- Update velocities
  let outerLoop i
        | i >= numBodies = return ()
        | otherwise = do
            ix <- readArray arr (i * bodySize + bX)
            iy <- readArray arr (i * bodySize + bY)
            iz <- readArray arr (i * bodySize + bZ)
            im <- readArray arr (i * bodySize + bMass)
            let innerLoop j
                  | j >= numBodies = return ()
                  | otherwise = do
                      jx <- readArray arr (j * bodySize + bX)
                      jy <- readArray arr (j * bodySize + bY)
                      jz <- readArray arr (j * bodySize + bZ)
                      jm <- readArray arr (j * bodySize + bMass)
                      let dx = ix - jx
                          dy = iy - jy
                          dz = iz - jz
                          dSq = dx * dx + dy * dy + dz * dz
                          dist = sqrt dSq
                          mag = dt / (dSq * dist)
                      ivx <- readArray arr (i * bodySize + bVX)
                      ivy <- readArray arr (i * bodySize + bVY)
                      ivz <- readArray arr (i * bodySize + bVZ)
                      writeArray arr (i * bodySize + bVX) (ivx - dx * jm * mag)
                      writeArray arr (i * bodySize + bVY) (ivy - dy * jm * mag)
                      writeArray arr (i * bodySize + bVZ) (ivz - dz * jm * mag)
                      jvx <- readArray arr (j * bodySize + bVX)
                      jvy <- readArray arr (j * bodySize + bVY)
                      jvz <- readArray arr (j * bodySize + bVZ)
                      writeArray arr (j * bodySize + bVX) (jvx + dx * im * mag)
                      writeArray arr (j * bodySize + bVY) (jvy + dy * im * mag)
                      writeArray arr (j * bodySize + bVZ) (jvz + dz * im * mag)
                      innerLoop (j + 1)
            innerLoop (i + 1)
            outerLoop (i + 1)
  outerLoop 0
  -- Update positions
  forM_ [0 .. numBodies - 1] $ \i -> do
    x  <- readArray arr (i * bodySize + bX)
    y  <- readArray arr (i * bodySize + bY)
    z  <- readArray arr (i * bodySize + bZ)
    vx <- readArray arr (i * bodySize + bVX)
    vy <- readArray arr (i * bodySize + bVY)
    vz <- readArray arr (i * bodySize + bVZ)
    writeArray arr (i * bodySize + bX) (x + dt * vx)
    writeArray arr (i * bodySize + bY) (y + dt * vy)
    writeArray arr (i * bodySize + bZ) (z + dt * vz)

energy :: IOUArray Int Double -> IO Double
energy arr = do
  let outerLoop i acc
        | i >= numBodies = return acc
        | otherwise = do
            vx <- readArray arr (i * bodySize + bVX)
            vy <- readArray arr (i * bodySize + bVY)
            vz <- readArray arr (i * bodySize + bVZ)
            m  <- readArray arr (i * bodySize + bMass)
            let ke = 0.5 * m * (vx * vx + vy * vy + vz * vz)
            ix <- readArray arr (i * bodySize + bX)
            iy <- readArray arr (i * bodySize + bY)
            iz <- readArray arr (i * bodySize + bZ)
            let innerLoop j pe
                  | j >= numBodies = return pe
                  | otherwise = do
                      jx <- readArray arr (j * bodySize + bX)
                      jy <- readArray arr (j * bodySize + bY)
                      jz <- readArray arr (j * bodySize + bZ)
                      jm <- readArray arr (j * bodySize + bMass)
                      let dx = ix - jx
                          dy = iy - jy
                          dz = iz - jz
                          dist = sqrt (dx * dx + dy * dy + dz * dz)
                      innerLoop (j + 1) (pe - m * jm / dist)
            pe <- innerLoop (i + 1) 0
            outerLoop (i + 1) (acc + ke + pe)
  outerLoop 0 0

benchmarkNBody :: Int -> IO Double
benchmarkNBody steps = do
  sys <- createSystem
  let loop 0 = return ()
      loop n = advance sys 0.01 >> loop (n - 1)
  loop steps
  energy sys

verify :: Double -> Bool
verify e = abs (e - (-0.1690859889909308)) < 1.0e-8

main :: IO ()
main = do
  args <- getArgs
  let numIters   = case args of { (n:_) -> read n; _ -> 3 }
      innerIters = case args of { (_:i:_) -> read i; _ -> 250000 }
  totalUs <- runBench numIters innerIters
  putStrLn $ "Total Runtime: " ++ show totalUs ++ "us"

runBench :: Int -> Int -> IO Integer
runBench numIters inner = go numIters 0
  where
    go 0 !acc = return acc
    go n !acc = do
      t0 <- getMonotonicTimeNSec
      !result <- benchmarkNBody inner
      when (not (verify result)) $
        putStrLn $ "ERROR: verification failed, energy = " ++ show result
      t1 <- getMonotonicTimeNSec
      let elapsed = fromIntegral (t1 - t0) `div` 1000
      go (n - 1) (acc + elapsed)
