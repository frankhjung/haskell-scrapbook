module Main(main) where

import           Criterion.Main (bench, defaultMain, whnf)
import           MonTrans       (sumTillNegative, sumTillNegative',
                                 sumTillNegative'')

main :: IO ()
main = defaultMain
  [ bench "sumTillNegative" $ whnf sumTillNegative [1,2,3,4,5,6,7,8,9,10,-1,11]
  , bench "sumTillNegative'" $ whnf sumTillNegative' [1,2,3,4,5,6,7,8,9,10,-1,11]
  , bench "sumTillNegative''" $ whnf sumTillNegative'' [1,2,3,4,5,6,7,8,9,10,-1,11]
  ]
