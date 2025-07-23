module Main(main) where

import           Criterion.Main (bench, defaultMain, whnf)
import           MySum          (mySum)

main :: IO ()
main = defaultMain
  [
    bench "sum"  $ whnf sum   [1..1000]
  , bench "mySum" $ whnf mySum [1..1000]
  ]
