module MyReverseSpec (spec) where

import           MyReverse       (myRevl, myRevr, myRevr2)
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck

-- | Head of myRevl is same as last
prop_myRevl :: NonEmptyList [Int] -> Bool
prop_myRevl (NonEmpty xs) = (head . myRevl) xs == last xs

-- | myRevl is same as myRevr
prop_myRevl_myRevr :: [Int] -> Bool
prop_myRevl_myRevr xs = myRevl xs == myRevr xs

-- | myRevr is same as myRevr2
prop_myRevr_myRevr2 :: [Int] -> Bool
prop_myRevr_myRevr2 xs = myRevr xs == myRevr2 xs

spec :: Spec
spec =
  describe "last and penultimate of lists" $ do
    it "head . myRevl is last" $
      quickCheck prop_myRevl
    it "myRevl is myRevr" $
      quickCheck prop_myRevl_myRevr
    it "myRevr is myRevr2" $
      quickCheck prop_myRevr_myRevr2
