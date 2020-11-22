{-# LANGUAGE ScopedTypeVariables #-}

module MonTransSpec (spec) where

import           MonTrans              (sumTillNegative, sumTillNegative',
                                        sumTillNegative'')

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)

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
    it "sumTillNegative'' equals 6" $
      sumTillNegative'' [1, 2, 3, -1, 4] `shouldBe` 6
    prop "sumTillNegative same as sumTillNegative'" $
      \(xs :: [Int]) -> sumTillNegative xs == sumTillNegative' xs
    prop "sumTillNegative same as sumTillNegative''" $
      \(xs :: [Int]) -> sumTillNegative xs == sumTillNegative'' xs

