module RandomSpec (spec) where

import           Data.Ix                 (inRange)
import           Random                  (dice, roll, rolls, seed)
import           Test.Hspec              (Spec, describe, it, shouldBe)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

-- | Random dice rolls are bounded in range 1..6
prop_random_dice :: Positive Int -> Property
prop_random_dice (Positive n) = monadicIO $ do
  ns <- run (roll n)
  assert (all isBounded ns)
  where
    isBounded = inRange dice

spec :: Spec
spec =
  describe "random with seed" $ do
    it "roll dice t times with seed" $
      take 5 (rolls (seed 111111)) `shouldBe` ([4,6,5,3,2] :: [Int])
    it "dice rolls are bounded" $
      quickCheck prop_random_dice
