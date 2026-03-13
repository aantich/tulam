-- AWFY Mandelbrot benchmark — Mandelbrot set with bit packing
-- Ported from the SOM benchmarks (Stefan Marr)
module Main where

import Control.Monad (when)
import Data.Bits (shiftL, xor)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)

mandelbrot :: Int -> Int
mandelbrot size = go 0 0 0 0
  where
    fsize = fromIntegral size :: Double

    go !y !sum !byteAcc !bitNum
      | y >= size = sum
      | otherwise = goX y 0 sum byteAcc bitNum

    goX !y !x !sum !byteAcc !bitNum
      | x >= size = go (y + 1) sum byteAcc bitNum
      | otherwise =
          let ci = (2.0 * fromIntegral y / fsize) - 1.0
              cr = (2.0 * fromIntegral x / fsize) - 1.5
              escape = iterMandel cr ci 0.0 0.0 0.0 50
              byteAcc' = shiftL byteAcc 1 + escape
              bitNum'  = bitNum + 1
          in if bitNum' == 8
             then goX y (x + 1) (sum `xor` byteAcc') 0 0
             else if x == size - 1
                  then let byteAcc'' = shiftL byteAcc' (8 - bitNum')
                       in goX y (x + 1) (sum `xor` byteAcc'') 0 0
                  else goX y (x + 1) sum byteAcc' bitNum'

    -- Matches JS exactly: compute then check, 50 times
    iterMandel !cr !ci !zrzr !zizi !zi !n
      | n <= 0    = 0
      | otherwise =
          let zr    = zrzr - zizi + cr
              zi'   = 2.0 * zr * zi + ci
              zrzr' = zr * zr
              zizi' = zi' * zi'
          in if zrzr' + zizi' > 4.0
             then 1
             else iterMandel cr ci zrzr' zizi' zi' (n - 1 :: Int)

benchmark :: Int -> Int
benchmark inner = mandelbrot inner

verify :: Int -> Int -> Bool
verify 500 r = r == 191
verify 750 r = r == 50
verify 1   r = r == 128
verify _   _ = False

main :: IO ()
main = do
  args <- getArgs
  let numIters   = case args of { (n:_) -> read n; _ -> 3 }
      innerIters = case args of { (_:i:_) -> read i; _ -> 500 }
  totalUs <- runBench numIters innerIters
  putStrLn $ "Total Runtime: " ++ show totalUs ++ "us"

runBench :: Int -> Int -> IO Integer
runBench numIters inner = go numIters 0
  where
    go 0 !acc = return acc
    go n !acc = do
      t0 <- getMonotonicTimeNSec
      let !result = benchmark inner
      when (not (verify inner result)) $
        putStrLn $ "ERROR: verification failed, got " ++ show result
      t1 <- getMonotonicTimeNSec
      let elapsed = fromIntegral (t1 - t0) `div` 1000
      go (n - 1) (acc + elapsed)
