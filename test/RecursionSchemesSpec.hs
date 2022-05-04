{-# LANGUAGE ScopedTypeVariables #-}
module RecursionSchemesSpec (spec) where

import           Numeric.Natural       (Natural)
import           RecursionSchemes      (Fix (..), ListF (..), buildListF,
                                        fromNat, insert, lengthListF,
                                        lengthListF', toList, toNat)

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (NonNegative (..))

-- list test value
ls :: Fix (ListF Int)
ls = Fix (ConsF 4 (Fix (ConsF 3 (Fix (ConsF 2 (Fix (ConsF 1 (Fix NilF))))))))

spec :: Spec
spec = do
  describe "anamorphism" $ do
    it "build list has length of 4" $
      (lengthListF . buildListF) 4 `shouldBe` 4
    it "build list same as constant list" $
      toList (buildListF 4) `shouldBe` toList ls
    it "build list equals constant list" $
      buildListF 4 `shouldBe` ls
  describe "catamorphism" $ do
    it "have length of 4" $
      lengthListF ls `shouldBe` 4
    prop "natural to integer" $
      \(NonNegative (i :: Int)) -> fromNat (toNat i) `shouldBe` i
  describe "paramorphism" $
    it "have length of 4" $
      lengthListF' ls `shouldBe` 4
  describe "quickcheck random list generation" $
    prop "quickcheck list length same as list build" $
      \(NonNegative (n :: Int)) -> (lengthListF . buildListF) n `shouldBe` n
  describe "para'" $ do
    it "insert 1 [] is [1]" $
      insert 1 [] `shouldBe` [1]
    it "insert c abde is abcde" $
      insert 'c' "abde" `shouldBe` "abcde"
