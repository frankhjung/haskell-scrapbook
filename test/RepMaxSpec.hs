{-# LANGUAGE ScopedTypeVariables #-}

module RepMaxSpec (spec) where

import           RepMax                (doRepMax, foldMax, traverseMax,
                                        traverseMax')

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)

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
    prop "quickcheck doRepMax is same as foldMax" $
      \(ys :: [Int]) -> doRepMax ys == foldMax ys
    prop "quickcheck foldMax is same as traverseMax" $
      \(ys :: [Int]) -> foldMax ys == traverseMax ys
    prop "quickcheck foldMax is idempotent" $
      \(ys :: [Int]) -> foldMax (foldMax ys) == foldMax ys
    prop "quickcheck traverseMax is same as traverseMax'" $
      \(ys :: [Int]) -> traverseMax ys == traverseMax' ys
    prop "quickcheck traverseMax is idempotent" $
      \(ys :: [Int]) -> traverseMax (traverseMax ys) == traverseMax ys
    prop "quickcheck traverseMax' is idempotent" $
      \(ys :: [Int]) -> traverseMax' (traverseMax' ys) == traverseMax' ys
