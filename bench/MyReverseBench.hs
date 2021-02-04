module Main(main) where

import           Criterion.Main (bench, defaultMain, whnf)
import           MyReverse      (myRevl, myRevr, myRevr2)

main :: IO ()
main = defaultMain
  [
    bench "myRevl"  $ whnf myRevl  "abcdefg"
  , bench "myRevr"  $ whnf myRevr  "abcdefg"
  , bench "myRevr2" $ whnf myRevr2 "abcdefg"
  ]
