module PolyDivisorsSpec (spec) where

import           PolyDivisors (findPolyDiv, isPolyMod, isPolyMod', isPolyMod'')

import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "find poly divisors" $ do
    it "poly divisors of 123" $ findPolyDiv 123 `shouldBe` [123,321]
    it "poly divisors of 1234" $ findPolyDiv 1234 `shouldBe` []
    it "poly divisors of 12345" $ findPolyDiv 12345 `shouldBe` []
    it "poly divisors of 123456" $ findPolyDiv 123456 `shouldBe` [123654,321654]
    it "is poly mod" $ isPolyMod 123 `shouldBe` True
    it "is poly mod'" $ isPolyMod' 123 `shouldBe` True
    it "is poly mod''" $ isPolyMod'' 123 `shouldBe` True
