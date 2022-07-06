module ApplyToTupleSpec (spec) where

import           ApplyToTuple (applyToTuple)

import           Test.Hspec   (Spec, describe, it, shouldBe)


spec :: Spec
spec =
  describe "apply length to tuple" $
    it "length 3 4" $ do
      let xs = "foo" :: String
          ys = [1..4] :: [Word]
      applyToTuple length (xs, ys) `shouldBe` (3,4)
