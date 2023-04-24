-- {-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}


{-|

Module      : MyJson
Description : Decode JSON
Copyright   : © Frank Jung, 2023
License     : GPL-3

Decode JSON which contains special characters like '°' (ASCII decimal \176).

A modified decode function to parse a ByteString containing special characters:

import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder(toLazyByteString)
import Data.Aeson (decode)

decodeUtf8 :: FromJSON a => Text -> Maybe a
decodeUtf8 = decode . toLazyByteString . encodeUtf8Builder

-}

module MyJson ( MyJson (..)
              , eitherDecodeUft8
              ) where

import           Data.Aeson                    (FromJSON, Value (..),
                                                eitherDecode, parseJSON, (.:))
-- import           Data.ByteString.Builder (toLazyByteString)
import           Data.ByteString.Lazy.Internal (ByteString (..))
import           Data.Text                     (Text)
import           Data.Text.Lazy.Encoding       (decodeLatin1, encodeUtf8)


-- | Define a test data type.
-- TODO parse a datetime string.
data MyJson = MyJson
  { name       :: Text
  , identifier :: Int
  , modifier   :: Float
  } deriving stock (Eq, Show)

instance FromJSON MyJson where
  parseJSON (Object v) =  MyJson
                            <$> v .: "name"
                            <*> v .: "identifier"
                            <*> v .: "modifier"
  parseJSON _          = fail "Expected an object"

-- TODO write ToJSON instance.

{-

Data.Text.Lazy.pack :: String -> Text

Data.Text.Lazy.Encoding.decodeUtf8 :: ByteString -> Text
Data.Text.Lazy.Encoding.encodeUtf8 :: Text -> ByteString
Data.Aeson.eitherDecode :: ByteString -> Either String a

eitherDecodeUft8 = eitherDecode . toLazyByteString . encodeUtf8Builder
-}

-- | Decode a ByteString containing special characters such as '°' (ASCII decimal \176).
eitherDecodeUft8 :: FromJSON a => ByteString -> Either String a
eitherDecodeUft8 = eitherDecode . encodeUtf8 . decodeLatin1
