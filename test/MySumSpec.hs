module MySumSpec (spec) where

import           MySum                 (mySum)
import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (NonEmptyList (NonEmpty))

spec :: Spec
spec =
  describe "mySum" $ do
    it "computes the sum of a list of integers" $
      mySum [1, 2, 3, 4, 5] `shouldBe` 15
    it "returns 0 for an empty list" $
      mySum [] `shouldBe` 0
    prop "compute sum for non-empty lists" $
      \(NonEmpty xs) -> mySum xs == sum xs
