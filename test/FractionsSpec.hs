module FractionsSpec (spec) where

import           Fractions  (sumOfFractions)

import           Data.Ratio ((%))
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "fraction calculations" $
    it "calculates sum of fractions" $
      sumOfFractions `shouldBe` 27 % 25
