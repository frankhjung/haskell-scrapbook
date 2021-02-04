module Main(main) where

import           Criterion.Main (bench, defaultMain, whnf)
import qualified ZipFold        (zip)

main :: IO ()
main = defaultMain
  [
    bench "zipFold" $ whnf ZipFold.zip "abcde"
  , bench "zip"     $ whnf zip "abcde"
  ]
