{-# LANGUAGE ScopedTypeVariables #-}

module SplitListSpec (spec) where

import           SplitList             (splitMiddle)

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)

spec :: Spec
spec =
  describe "use zipWith to split a list in half" $ do
    it "expect (hello, world)" $
      splitMiddle "helloworld" `shouldBe` ("hello", "world")
    prop "quickcheck test split (xs ++ xs)  == (xs, xs)" $
     \(xs :: String) -> splitMiddle (xs ++ xs) == (xs, xs)
