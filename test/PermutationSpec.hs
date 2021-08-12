{-# LANGUAGE ScopedTypeVariables #-}

module PermutationSpec (spec) where

import           Permutation           (inserts, perms1, perms2, perms3, picks)

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSize, prop)

import           Data.List             (permutations, sort)

spec :: Spec
spec =
  describe "generate list permutations" $ do
    it "inserts 'a' \"bc\" is [\"abc\",\"bac\",\"bca\"]" $
      inserts 'a' "bc" `shouldBe` ["abc","bac","bca"]
    it "picks \"abc\" is [('a',\"bc\"),('b',\"ac\"),('c',\"ab\")]" $
      picks "abc" `shouldBe` [('a',"bc"),('b',"ac"),('c',"ab")]
    modifyMaxSize (const 7) $
      prop "perms1 same as Data.List permutations" $
        \(x :: String) -> (sort . perms1) x == (sort . permutations) x
    modifyMaxSize (const 7) $
      prop "perms2 same as Data.List permutations" $
        \(x :: String) -> (sort . perms2) x == (sort . permutations) x
    modifyMaxSize (const 7) $
      prop "perms3 same as Data.List permutations" $
        \(x :: String) -> (sort . perms3) x == (sort . permutations) x

