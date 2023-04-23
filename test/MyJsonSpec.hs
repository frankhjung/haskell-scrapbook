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

import           MyJson                        (MyJson (..))

jsonByteString :: ByteString
jsonByteString = "{\"name\":\"Frank\",\"identifier\":1}"

spec :: Spec
spec =
  describe "test decode utf-8 input" $
    it "returns an instance of MyJson type" $
      eitherDecode jsonByteString `shouldBe` Right (MyJson "Frank" 1)
