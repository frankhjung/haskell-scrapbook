{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Can encode and decode when ByteString does not contain special characters like
'°' (ASCII decimal \176).
-}

module MyJsonSpec (spec) where

import           Data.Aeson                    (eitherDecode, encode)
import           Data.ByteString.Lazy.Internal (ByteString (..))

import           Test.Hspec                    (Spec, describe, it, shouldBe)

import           MyJson                        (MyJson (..),
                                                eitherDecodeSpecial,
                                                encodeSpecial)

jsonRegularString :: ByteString
jsonRegularString = "{\"name\":\"Frank\"\
                    \,\"identifier\":1\
                    \,\"modifier\":2.14}"

jsonSpecialString :: ByteString
jsonSpecialString = "{\"name\":\"François\"\
                    \,\"identifier\":2\
                    \,\"modifier\":3.14}"

myJsonRegular :: MyJson
myJsonRegular = MyJson "Frank" 1 2.14

myJsonSpecial :: MyJson
myJsonSpecial = MyJson "François" 2 3.14

spec :: Spec
spec = do
  describe "test decode" $ do
    it "decode regular byte string" $
      eitherDecode jsonRegularString `shouldBe` Right myJsonRegular
    it "decode special byte string" $
      eitherDecodeSpecial jsonSpecialString `shouldBe` Right myJsonSpecial
  describe "test encode" $ do
    it "encode regular" $
      (eitherDecode . encode) myJsonRegular `shouldBe` Right myJsonRegular
    it "encode special" $
      (eitherDecode . encodeSpecial) myJsonSpecial `shouldBe` Right myJsonSpecial
