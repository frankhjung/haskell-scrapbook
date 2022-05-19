module Main(main) where

import           Criterion.Main   (bench, defaultMain, whnf)
import           RecursionSchemes (idx0, idx1, idx2, idx3, idx4)

main :: IO ()
main = defaultMain
  [
    bench "idx0" $ whnf idx0 ['a'..'z']
  , bench "idx1" $ whnf idx1 ['a'..'z']
  , bench "idx2" $ whnf idx2 ['a'..'z']
  , bench "idx3" $ whnf idx3 ['a'..'z']
  , bench "idx4" $ whnf idx4 ['a'..'z']
  ]
