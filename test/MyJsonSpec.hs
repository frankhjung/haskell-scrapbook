{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Can encode and decode when ByteString does not contain special characters like
'°' (ASCII decimal \176).
-}

module MyJsonSpec (spec) where

import           Data.Aeson                    (eitherDecode)
import           Data.ByteString.Lazy.Internal (ByteString (..))

import           Test.Hspec                    (Spec, describe, it, shouldBe)

import           MyJson                        (MyJson (..), eitherDecodeUft8)

jsonByteString :: ByteString
jsonByteString = "{ \"name\":\"Frank\"\
                 \,\"identifier\":1\
                 \,\"modifier\":2.14}"

jsonSpecialString :: ByteString
jsonSpecialString = "{ \"name\":\"François\"\
                    \,\"identifier\":1\
                    \,\"modifier\":2.14}"

spec :: Spec
spec =
  describe "test decode utf-8 input" $ do
    it "regular byte string" $
      eitherDecode jsonByteString `shouldBe` Right (MyJson "Frank" 1 2.14)
    it "special byte string" $
       eitherDecodeUft8 jsonSpecialString `shouldBe` Right (MyJson "François" 1 2.14)
