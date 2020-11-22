{-# LANGUAGE ScopedTypeVariables #-}

module RandomSpec (spec) where

import           Data.Ix                 (inRange)
import           Random                  (dice, roll, rolls, seed)
import           Test.Hspec              (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck         (Positive (..))
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

spec :: Spec
spec =
  describe "random with seed" $ do
    it "roll dice t times with seed" $
      take 5 (rolls (seed 111111)) `shouldBe` ([4,6,5,3,2] :: [Int])
    prop "dice rolls are bounded in range 1..6" $
      \(Positive (n :: Int)) -> monadicIO $ do
        ns <- run (roll n)
        assert (all isBounded ns)
        where
          isBounded = inRange dice

