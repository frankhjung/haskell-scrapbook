module Main(main) where

import           Criterion.Main (bench, bgroup, defaultMain, whnf)
import           SubSeqs        (subSeqs1, subSeqs2, subSeqs3, subSeqs4)

import           RepMax         (doRepMax, foldMax, traverseMax, traverseMax')

main :: IO ()
main = defaultMain
  [
    bgroup "SubSeqs"
    [
      bench "subSeqs1" $ whnf subSeqs1 "abc"
    , bench "subSeqs2" $ whnf subSeqs2 "abc"
    , bench "subSeqs3" $ whnf subSeqs3 "abc"
    , bench "subSeqs4" $ whnf subSeqs4 "abc"
    ],
    bgroup "RepMax"
    [
      bench "doRepMax" $ whnf doRepMax ([2,3,1,4,5] :: [Int])
    , bench "foldMax" $ whnf foldMax ([2,3,1,4,5] :: [Int])
    , bench "traverseMax" $ whnf traverseMax ([2,3,1,4,5] :: [Int])
    , bench "traverseMax'" $ whnf traverseMax' ([2,3,1,4,5] :: [Int])
    ]
  ]
