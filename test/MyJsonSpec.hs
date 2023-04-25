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

import           Data.Time.Calendar            (fromGregorian)
import           Data.Time.Clock               (UTCTime (..))
import           MyJson                        (MyJson (..),
                                                eitherDecodeSpecial,
                                                encodeSpecial)

myDate :: UTCTime
myDate = UTCTime (fromGregorian 2019 12 31) (3600*12)

jsonRegularString :: ByteString
jsonRegularString = "{\"name\":\"Frank\"\
                    \,\"identifier\":1\
                    \,\"modifier\":2.14\
                    \,\"created\":\"2019-12-31T12:00:00Z\"\
                    \,\"series\":[1,2,3,4,5]}"

jsonSpecialString :: ByteString
jsonSpecialString = "{\"name\":\"François\"\
                    \,\"identifier\":2\
                    \,\"modifier\":3.14\
                    \,\"created\":\"2019-12-31T12:00:00Z\"\
                    \,\"series\":[6,7,9,10]}"

myJsonRegular :: MyJson
myJsonRegular = MyJson "Frank" 1 2.14 myDate [1,2,3,4,5]

myJsonSpecial :: MyJson
myJsonSpecial = MyJson "François" 2 3.14 myDate [6,7,9,10]

spec :: Spec
spec = do
  describe "test decode" $ do
    it "decode regular" $
      eitherDecode jsonRegularString `shouldBe` Right myJsonRegular
    it "decode special" $
      eitherDecodeSpecial jsonSpecialString `shouldBe` Right myJsonSpecial
  describe "test encode" $ do
    it "encode regular" $
      (eitherDecode . encode) myJsonRegular `shouldBe` Right myJsonRegular
    it "encode special" $
      (eitherDecode . encodeSpecial) myJsonSpecial `shouldBe` Right myJsonSpecial
    it "encode special string using regular" $
      (eitherDecode . encode) myJsonSpecial `shouldBe` Right myJsonSpecial
