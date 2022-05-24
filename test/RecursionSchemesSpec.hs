{-# LANGUAGE ScopedTypeVariables #-}
module RecursionSchemesSpec (spec) where

import           Data.List                 as DL (insert, tails)
import           RecursionSchemes          (Fix (..), ListF (..), buildListF,
                                            fromNat, idx0, idx1, idx2, idx3,
                                            idx4, lengthListF, lengthListF',
                                            para', para'', toList, toNat)
import           RecursionSchemes          as RS (insert, insert')

import           Test.Hspec                (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           (NonNegative (..))
import           Test.QuickCheck.Modifiers (NonEmptyList (NonEmpty))

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
    it "has length of 4" $
      lengthListF ls `shouldBe` 4
    prop "natural to integer" $
      \(NonNegative (i :: Int)) -> fromNat (toNat i) `shouldBe` i
    prop "quickcheck list length same as list build" $
      \(NonNegative (n :: Int)) -> (lengthListF . buildListF) n `shouldBe` n

  describe "paramorphism (insert)" $ do
    prop "insert x [] is [x]" $
      \(x :: Char) -> RS.insert x [] `shouldBe` [x]
    prop "insert same as Data.List.insert" $
      \(x :: Char, xs :: String) -> RS.insert x xs `shouldBe` DL.insert x xs

  describe "paramorphism (insert')" $ do
    prop "insert' x [] is [x]" $
      \(x :: Char) -> RS.insert' x [] `shouldBe` [x]
    prop "insert' same as Data.List.insert" $
      \(x :: Char, xs :: String) -> RS.insert' x xs `shouldBe` DL.insert x xs

  describe "paramorphism (para)" $
    it "has length of 4" $
      lengthListF' ls `shouldBe` 4

  describe "paramorphism (para')" $ do
    prop "para' to sum lists" $
      \(xs :: [Int]) -> sum xs `shouldBe` para' (const . (+)) 0 xs
    prop "para' to produce all suffixes" $
      \(NonEmpty (xs :: String)) -> para' (const (:)) [] xs `shouldBe` (tail . DL.tails) xs

  describe "paramorphism (para'')" $ do
    prop "para'' to sum lists" $
      \(xs :: [Int]) -> sum xs `shouldBe` para'' (const . (+)) 0 xs
    prop "para'' to produce all suffixes" $
      \(NonEmpty (xs :: String)) -> para'' (const (:)) [] xs `shouldBe` (tail . DL.tails) xs

  describe "index list using fold" $ do
    prop "idx0 same as idx1" $
      \(xs :: String) -> (idx0 xs :: [Integer]) `shouldBe` (idx1 xs :: [Integer])
    prop "idx0 same as idx2" $
      \(xs :: String) -> idx0 xs `shouldBe` idx2 xs
    prop "idx0 same as idx3" $
      \(xs :: String) -> idx0 xs `shouldBe` idx3 xs
    prop "idx0 same as idx4" $
      \(xs :: String) -> idx0 xs `shouldBe` idx4 xs
