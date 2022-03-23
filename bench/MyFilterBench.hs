module Main(main) where

import           Criterion.Main (bench, defaultMain, whnf)
import           Data.Char      (isLower)
import           MyFilter       (myFilter)

main :: IO ()
main = defaultMain
  [
    bench "myFilter" $ whnf (myFilter isLower) "hHello wWorld"
  , bench "filter"   $ whnf (filter isLower) "hHello wWorld"
  ]
