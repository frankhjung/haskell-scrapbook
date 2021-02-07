module Main(main) where

import           Criterion.Main (bench, defaultMain, whnf)
import           Multiply       (multiply0, multiply1, multiply2, multiply3,
                                 multiply4)

main :: IO ()
main = defaultMain
  [
    bench "multiply0" $ whnf multiply0 15
  , bench "multiply1" $ whnf multiply1 15
  , bench "multiply2" $ whnf multiply2 15
  , bench "multiply3" $ whnf multiply3 15
  , bench "multiply4" $ whnf multiply4 15
  ]
