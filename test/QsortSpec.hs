{-# LANGUAGE ScopedTypeVariables #-}

module QsortSpec (spec) where

import           Data.List                 (sort)
import           Qsort                     (qsort)
import           Test.Hspec                (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck.Modifiers (NonEmptyList (NonEmpty))

spec :: Spec
spec =
  describe "qsort inefficient version" $ do
    it "qsort example" $
      qsort ([1,3,5,1,4,2] :: [Int]) `shouldBe` ([1,1,2,3,4,5] :: [Int])
    prop "qsort same as Data.List.sort" $
        \(NonEmpty (xs :: [Int])) -> qsort xs == sort xs
    prop "qsort is idempotent" $
        \(NonEmpty (xs :: [Int])) -> qsort (qsort xs) == qsort xs

