{-# LANGUAGE TypeApplications #-}

module MyTypeSpec (spec) where

import           MyType     (typeName)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "ambigous types" $ do
    it "typeName @Int is 'Int'" $
      typeName @Int `shouldBe` "Int"
    it "typeName @String is '[Char]'" $
      typeName @String `shouldBe` "[Char]"
    it "typeName @(Maybe Bool) is 'Maybe Bool'" $
      typeName @(Maybe Bool) `shouldBe` "Maybe Bool"
