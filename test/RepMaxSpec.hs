module RepMaxSpec (spec) where

import           RepMax          (doRepMax, foldMax, traverseMax, traverseMax')

import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck

-- | doRepMax is same as foldMax
prop_doRepMax_foldMax :: [Int] -> Bool
prop_doRepMax_foldMax xs = doRepMax xs == foldMax xs

-- | foldMax is same as traverseMax
prop_foldMax_traverseMax :: [Int] -> Bool
prop_foldMax_traverseMax xs = foldMax xs == traverseMax xs

-- | traverseMax is same as traverseMax'
prop_traverseMax_traverseMax' :: [Int] -> Bool
prop_traverseMax_traverseMax' xs = traverseMax xs == traverseMax' xs

-- | traverseMax is idempotent
prop_traverseMax :: [Int] -> Property
prop_traverseMax xs = traverseMax (traverseMax xs) === traverseMax xs

spec :: Spec
spec =
  describe "replace list with maximum element" $ do
    let xs = [-2,-3,-1,-4,-5] :: [Int]
    it "doRepMax is same as foldMax" $
      doRepMax xs `shouldBe` foldMax xs
    it "foldMax is same as traverseMax" $
      foldMax xs `shouldBe` traverseMax xs
    it "traverseMax is same as traverseMax'" $
      traverseMax xs `shouldBe` traverseMax' xs
    it "quickcheck doRepMax is same as foldMax" $
      quickCheck prop_doRepMax_foldMax
    it "quickcheck foldMax is same as traverseMax" $
      quickCheck prop_foldMax_traverseMax
    it "quickcheck traverseMax is same as traverseMax'" $
      quickCheck prop_traverseMax_traverseMax'
    it "quickcheck traverseMax is idempotent" $
      quickCheck prop_traverseMax
