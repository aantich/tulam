-- AWFY List benchmark — recursive linked-list manipulation
-- Ported from the SOM benchmarks (Stefan Marr)
module Main where

import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)

data Element = Nil | Cons !Int Element

makeList :: Int -> Element
makeList 0 = Nil
makeList n = Cons n (makeList (n - 1))

listLength :: Element -> Int
listLength Nil         = 0
listLength (Cons _ xs) = 1 + listLength xs

isShorterThan :: Element -> Element -> Bool
isShorterThan _        Nil        = False
isShorterThan Nil      (Cons _ _) = True
isShorterThan (Cons _ xs) (Cons _ ys) = isShorterThan xs ys

tailFn :: Element -> Element -> Element -> Element
tailFn x y z
  | isShorterThan y x =
      tailFn (tailFn (tl x) y z)
             (tailFn (tl y) z x)
             (tailFn (tl z) x y)
  | otherwise = z
  where
    tl Nil        = Nil
    tl (Cons _ r) = r

benchmark :: Int -> Int
benchmark _ =
  let x = makeList 15
      y = makeList 10
      z = makeList 6
  in listLength (tailFn x y z)

verify :: Int -> Bool
verify r = r == 10

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
      if not (verify result)
        then putStrLn "ERROR: verification failed"
        else return ()
      t1 <- getMonotonicTimeNSec
      let elapsed = fromIntegral (t1 - t0) `div` 1000
      go (n - 1) (acc + elapsed)
