module MyPenultimateSpec (spec) where

import           MyPenultimate (penultimate)
import           Test.Hspec    (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "penultimate of lists" $ do
    it "penultimate [] is Nothing" $
      penultimate ([] :: [Int]) `shouldBe` Nothing
    it "penultimate [1] is Nothing" $
      penultimate ([1] :: [Int]) `shouldBe` Nothing
    it "penultimate [1,2] is 1" $
      penultimate ([1,2] :: [Int]) `shouldBe` Just 1
