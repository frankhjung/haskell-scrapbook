module Main(main) where

import           Criterion.Main (bench, bgroup, defaultMain, whnf)
import           MyReverse      (myRevl, myRevr, myRevr2)
import           PolyDivisors   (isPolyMod, isPolyMod', isPolyMod'')
import           RepMax         (doRepMax, foldMax, traverseMax, traverseMax')
import           SubSeqs        (subSeqs1, subSeqs2, subSeqs3, subSeqs4)
import qualified ZipFold        (zip)

main :: IO ()
main = defaultMain
  [
    bgroup "MyReverse"
    [
      bench "myRevl"  $ whnf myRevl "abcdefg"
    , bench "myRevr"  $ whnf myRevr "abcdefg"
    , bench "myRevr2" $ whnf myRevr2 "abcdefg"
    ],
    bgroup "PolyDivisors"
    [
      bench "isPolyMod"   $ whnf isPolyMod    1234
    , bench "isPolyMod'"  $ whnf isPolyMod'   1234
    , bench "isPolyMod''" $ whnf isPolyMod''  1234
    ],
    bgroup "RepMax"
    [
      bench "doRepMax"     $ whnf doRepMax ([2,3,1,4,5] :: [Int])
    , bench "foldMax"      $ whnf foldMax ([2,3,1,4,5] :: [Int])
    , bench "traverseMax"  $ whnf traverseMax ([2,3,1,4,5] :: [Int])
    , bench "traverseMax'" $ whnf traverseMax' ([2,3,1,4,5] :: [Int])
    ],
    bgroup "SubSeqs"
    [
      bench "subSeqs1" $ whnf subSeqs1 "abc"
    , bench "subSeqs2" $ whnf subSeqs2 "abc"
    , bench "subSeqs3" $ whnf subSeqs3 "abc"
    , bench "subSeqs4" $ whnf subSeqs4 "abc"
    ],
    bgroup "ZipFold"
    [
      bench "zipFold" $ whnf ZipFold.zip "abcde"
    , bench "zip" $ whnf zip "abcde"
    ]
  ]
