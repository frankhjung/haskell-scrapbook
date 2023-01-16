{-# LANGUAGE ScopedTypeVariables #-}

module MyFreeMonadSpec (spec) where

import           MyFreeMonad (evalArith, example)
import           Test.Hspec  (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "MyFreeMonad returns evaluated expression" $
    it "example" $
      evalArith (example 0) `shouldBe` 5

