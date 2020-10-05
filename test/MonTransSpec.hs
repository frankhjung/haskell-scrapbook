module MonTransSpec (spec) where

import           MonTrans        (sumTillNegative, sumTillNegative')

import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck


-- | sum till negative are equal
prop_list :: [Int] -> Bool
prop_list xs = sumTillNegative xs == sumTillNegative' xs

spec :: Spec
spec =
  describe "sumTillNegative a list of integers" $ do
    it "sumTillNegative equals 0" $
      sumTillNegative [] `shouldBe` 0
    it "sumTillNegative' equals 0" $
      sumTillNegative' [] `shouldBe` 0
    it "sumTillNegative equals 6" $
      sumTillNegative [1, 2, 3, -1, 4] `shouldBe` 6
    it "sumTillNegative' equals 6" $
      sumTillNegative' [1, 2, 3, -1, 4] `shouldBe` 6
    it "sumTillNegative same as sumTillNegative'" $
      quickCheckWith stdArgs prop_list

