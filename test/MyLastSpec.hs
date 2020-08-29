module MyLastSpec (spec) where

import           MyLast          (myLast, myRev1, myRev2, penultimate)
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck

-- | Head of myRev1 is same as last
prop_myRev1 :: NonEmptyList [Int] -> Bool
prop_myRev1 (NonEmpty xs) = (head . myRev1) xs == last xs

-- | myRev1 is same as myRev2
prop_myRev1_myRev2 :: [Int] -> Bool
prop_myRev1_myRev2 xs = myRev1 xs == myRev2 xs

spec :: Spec
spec =
  describe "last and penultimate of lists" $ do
    let xs = [1..5] :: [Int]
    it "myLast [1..5] is 5" $
      last xs `shouldBe` myLast xs
    it "myLast [4] is 4" $
      last ([4] :: [Int]) `shouldBe` myLast [4]
    it "penultimate [1] is Nothing" $
      penultimate ([1] :: [Int]) `shouldBe` Nothing
    it "penultimate [1,2] is 1" $
      penultimate ([1,2] :: [Int]) `shouldBe` Just 1
    it "head . myRev1 is last" $
      quickCheck prop_myRev1
    it "myRev1 is myRev2" $
      quickCheck prop_myRev1_myRev2
