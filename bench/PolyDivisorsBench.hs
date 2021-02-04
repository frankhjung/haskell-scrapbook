module Main(main) where

import           Criterion.Main (bench, defaultMain, whnf)
import           PolyDivisors   (isPolyMod, isPolyMod', isPolyMod'')

main :: IO ()
main = defaultMain
  [
    bench "isPolyMod"   $ whnf isPolyMod   1234
  , bench "isPolyMod'"  $ whnf isPolyMod'  1234
  , bench "isPolyMod''" $ whnf isPolyMod'' 1234
  ]
