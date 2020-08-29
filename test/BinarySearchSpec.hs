module BinarySearchSpec (spec) where

import           BinarySearch (bsearch)

import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "binary search" $
    it "find only 1 and 2" $ do
      let xs = [1, 2] :: [Int]
          ys = [3, 4] :: [Int]
      map (bsearch xs) (xs <> ys) `shouldBe` [Just 1, Just 2, Nothing, Nothing]
