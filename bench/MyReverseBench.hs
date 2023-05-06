module Main(main) where

import           Criterion.Main (bench, defaultMain, whnf)
import           MyReverse      (myRevRec, myRevl, myRevr)

main :: IO ()
main = defaultMain
  [
    bench "myRevl"  $ whnf myRevl  "abcdefg"
  , bench "myRevr"  $ whnf myRevr  "abcdefg"
  , bench "myRevRec" $ whnf myRevRec "abcdefg"
  ]
