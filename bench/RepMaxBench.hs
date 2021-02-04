module Main(main) where

import           Criterion.Main (bench, defaultMain, whnf)
import           RepMax         (doRepMax, foldMax, traverseMax, traverseMax')

main :: IO ()
main = defaultMain
  [
    bench "doRepMax"     $ whnf doRepMax     ([2,3,1,4,5] :: [Int])
  , bench "foldMax"      $ whnf foldMax      ([2,3,1,4,5] :: [Int])
  , bench "traverseMax"  $ whnf traverseMax  ([2,3,1,4,5] :: [Int])
  , bench "traverseMax'" $ whnf traverseMax' ([2,3,1,4,5] :: [Int])
  ]
