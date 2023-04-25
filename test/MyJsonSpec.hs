{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Can encode and decode when ByteString does not contain special characters like
'°' (ASCII decimal \176).
-}

module MyJsonSpec (spec) where

import           Data.Aeson                    (eitherDecode, encode)
import           Data.ByteString.Lazy.Internal (ByteString (..))

import           Data.Either                   (isLeft)
import           Test.Hspec                    (Spec, describe, it, shouldBe)

import           Data.Time.Calendar            (fromGregorian)
import           Data.Time.Clock               (UTCTime (..))
import           MyJson                        (MyJson (..),
                                                eitherDecodeSpecial,
                                                encodeSpecial)

myDate :: UTCTime
myDate = UTCTime (fromGregorian 2019 12 31) (3600*12)

-- From JSON string.
jsonRegularString :: ByteString
jsonRegularString = "{\"name\":\"Frank\"\
                    \,\"identifier\":1\
                    \,\"modifier\":2.14\
                    \,\"created\":\"2019-12-31T12:00:00Z\"\
                    \,\"series\":[1,2,3,4,5]}"

-- From JSON string.
jsonSpecialString :: ByteString
jsonSpecialString = "{\"name\":\"François\"\
                    \,\"identifier\":2\
                    \,\"modifier\":3.14\
                    \,\"created\":\"2019-12-31T12:00:00Z\"\
                    \,\"series\":[6,7,9,10]}"

-- To 'MyJson'.
myJsonRegular :: MyJson
myJsonRegular = MyJson "Frank" 1 2.14 myDate [1,2,3,4,5]

-- To 'MyJson'.
myJsonSpecial :: MyJson
myJsonSpecial = MyJson "François" 2 3.14 myDate [6,7,9,10]

spec :: Spec
spec = do
  describe "test decode" $ do
    it "decode regular" $
      eitherDecode jsonRegularString `shouldBe` Right myJsonRegular
    it "decode special" $
      eitherDecodeSpecial jsonSpecialString `shouldBe` Right myJsonSpecial
    it "decode special using default should fail" $
      isLeft (eitherDecode jsonSpecialString :: Either String MyJson)
  describe "test encode" $ do
    it "encode regular using encode" $
      (eitherDecode . encode) myJsonRegular `shouldBe` Right myJsonRegular
    it "encode special using encodeSpecial" $
      (eitherDecode . encodeSpecial) myJsonSpecial `shouldBe` Right myJsonSpecial
    it "encode special using encode" $
      (eitherDecode . encode) myJsonSpecial `shouldBe` Right myJsonSpecial
