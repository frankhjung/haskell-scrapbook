module PermutationSpec (spec) where

import           Permutation     (inserts, perms1)

import           Test.Hspec      (Spec, describe, it, shouldBe)

import           Data.List       (permutations, sort)

import           Test.QuickCheck

prop_permutations :: String -> Bool
prop_permutations xs = (sort . perms1) xs == (sort . permutations) xs

spec :: Spec
spec =
  describe "generate list permutations" $ do
    it "inserts 1 [2] is [[1,2], [2,1]]" $
      inserts 1 [2] `shouldBe` ([[1,2], [2,1]] :: [[Int]])
    it "perms1 \"abc\"" $
      perms1 "abc" `shouldBe` ["abc", "bac", "bca", "acb", "cab", "cba"]
    it "perms1 same as Data.List permutations" $
      quickCheckWith stdArgs { maxSize = 7 } prop_permutations

