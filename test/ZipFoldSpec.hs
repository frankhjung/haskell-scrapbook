module ZipFoldSpec (spec) where

import qualified ZipFold         (zip)

import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck

-- | Prelude.zip same as ZipFold.zip for lists
prop_zipfold :: NonEmptyList [Int] -> NonEmptyList String -> Property
prop_zipfold (NonEmpty xs) (NonEmpty ys) = ZipFold.zip xs ys === zip xs ys

spec :: Spec
spec =
  describe "ZipFold.zip is same as Prelude.zip" $
    it "expect same" $
      quickCheck prop_zipfold
