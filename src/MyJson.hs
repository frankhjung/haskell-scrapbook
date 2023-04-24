{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

{-|

Module      : MyJson
Description : Decode JSON
Copyright   : © Frank Jung, 2023
License     : GPL-3

Decode JSON which contains special characters like '°' (ASCII decimal \176).

== References

I found this a good guide when dealing with JSON and special characters:
https://guide.aelve.com/haskell/aeson-cookbook-amra6lk6

-}

module MyJson ( MyJson (..)
              , FromJSON
              , ToJSON
              , eitherDecodeSpecial
              , encodeSpecial
              ) where

import           Data.Aeson                    (FromJSON, ToJSON, Value (..),
                                                eitherDecode, object, parseJSON,
                                                toJSON, (.:), (.=))
import           Data.Aeson.Text               (encodeToLazyText)
import           Data.ByteString.Lazy.Internal (ByteString (..))
import           Data.Text                     (Text)
import           Data.Text.Lazy.Encoding       (decodeLatin1, encodeUtf8)
import           GHC.Generics                  (Generic)


-- | Define a test data type.
-- TODO parse a datetime string.
data MyJson = MyJson
  { name       :: Text
  , identifier :: Int
  , modifier   :: Float
  } deriving stock (Eq, Show, Generic)

instance FromJSON MyJson where
  parseJSON (Object v) =  MyJson
                            <$> v .: "name"
                            <*> v .: "identifier"
                            <*> v .: "modifier"
  parseJSON _          = fail "Expected an object"

-- TODO can we encode UTF-8 when encoding to JSON?
instance ToJSON MyJson where
  toJSON (MyJson _name _identifier _modifier) = object
    [ "name"       .= _name
    , "identifier" .= _identifier
    , "modifier"   .= _modifier
    ]

-- | Decode Special Characters.
--
-- Data.Text.Lazy.Encoding.decodeLatin1 :: ByteString -> Text
-- Data.Text.Lazy.Encoding.encodeUtf8 :: Text -> ByteString
-- Data.Aeson.eitherDecode :: ByteString -> Either String a
--
-- This will successfully decode a ByteString containing special characters such
-- as '°' (ASCII decimal \176).
eitherDecodeSpecial :: FromJSON a => ByteString -> Either String a
eitherDecodeSpecial = eitherDecode . encodeUtf8 . decodeLatin1

-- | Encode Special Characters.
--
-- Data.Aeson.Text.encodeToLazyText :: ToJSON a => a -> Text
-- Data.Text.Lazy.Encoding.encodeUtf8 :: Text -> ByteString
encodeSpecial :: ToJSON a => a -> ByteString
encodeSpecial = encodeUtf8 . encodeToLazyText
