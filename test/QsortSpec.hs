module QsortSpec (spec) where

import           Data.List       (sort)
import           Qsort           (qsort)
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck

-- | Qsort same as Data.List.sort
prop_qsort :: NonEmptyList [Int] -> Bool
prop_qsort (NonEmpty xs) = qsort xs == sort xs

-- | Qsort is idempotent
prop_qsort' :: NonEmptyList [Int] -> Property
prop_qsort' (NonEmpty xs) = qsort (qsort xs) === qsort xs

spec :: Spec
spec =
  describe "qsort inefficient version" $ do
    it "qsort example" $
      qsort ([1,3,5,1,4,2] :: [Int]) `shouldBe` ([1,1,2,3,4,5] :: [Int])
    it "qsort same as Data.List.sort" $
      quickCheck prop_qsort
    it "qsort is idempotent" $
      quickCheck prop_qsort'
