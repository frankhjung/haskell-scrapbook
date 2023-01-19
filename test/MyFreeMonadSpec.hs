{-# LANGUAGE ScopedTypeVariables #-}

module MyFreeMonadSpec (spec) where

import           MyFreeMonad (evalArith, example, example')
import           Test.Hspec  (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "MyFreeMonad returns evaluated expression" $ do
    it "example" $
      evalArith (example 0) `shouldBe` 5
    it "example'" $
      evalArith (example' 5) `shouldBe` 5
