module Mod35Spec (spec) where

import           Mod35      (mod35)

import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "modulus 3 or 5" $ do
    it "mod35 3 is True" $ mod35 3 `shouldBe` True
    it "mod35 5 is True" $ mod35 5 `shouldBe` True
    it "mod35 15 is True" $ mod35 15 `shouldBe` True
    it "mod35 8 is False" $ mod35 8 `shouldBe` False
