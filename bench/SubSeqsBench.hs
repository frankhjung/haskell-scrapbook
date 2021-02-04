module Main(main) where

import           Criterion.Main (bench, defaultMain, whnf)
import           SubSeqs        (subSeqs1, subSeqs2, subSeqs3, subSeqs4)

main :: IO ()
main = defaultMain
  [
    bench "subSeqs1" $ whnf subSeqs1 "abc"
  , bench "subSeqs2" $ whnf subSeqs2 "abc"
  , bench "subSeqs3" $ whnf subSeqs3 "abc"
  , bench "subSeqs4" $ whnf subSeqs4 "abc"
  ]
