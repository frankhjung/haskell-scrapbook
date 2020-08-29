module CpsSpec (spec) where

import           Control.Monad.Trans.Cont (runCont)
import           Cps                      (pythagorasCont)
import           Test.Hspec               (Spec, describe, it, shouldBe)
import           Test.QuickCheck

-- | Continuation Passing Style pythagoras
prop_pythagoras :: Int -> Int -> Bool
prop_pythagoras a b = runCont (pythagorasCont a b) Just == Just (a*a + b*b)

spec :: Spec
spec =
  describe "continuation passing style - cps" $ do
    it "pythagoras 3 4 is 25" $
      runCont (pythagorasCont 3 4) Just `shouldBe` Just 25
    it "pythagoras a b is a*a + b*b" $
      quickCheck prop_pythagoras
