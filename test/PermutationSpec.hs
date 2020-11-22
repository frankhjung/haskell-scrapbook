{-# LANGUAGE ScopedTypeVariables #-}

module PermutationSpec (spec) where

import           Permutation           (inserts, perms1)

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSize, prop)

import           Data.List             (permutations, sort)

spec :: Spec
spec =
  describe "generate list permutations" $ do
    it "inserts 1 [2] is [[1,2], [2,1]]" $
      inserts 1 [2] `shouldBe` ([[1,2], [2,1]] :: [[Int]])
    it "perms1 \"abc\"" $
      perms1 "abc" `shouldBe` ["abc", "bac", "bca", "acb", "cab", "cba"]
    modifyMaxSize (const 7) $
      prop "perms1 same as Data.List permutations" $
        \(x :: String) -> (sort . perms1) x == (sort . permutations) x

