-- {-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}


{-|

Module      : MyJson
Description : Decode JSON
Copyright   : © Frank Jung, 2023
License     : GPL-3

Decode JSON which contains special characters like '°' (ASCII decimal \176).

-}

module MyJson (MyJson (..)) where

import           Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
-- import           Data.ByteString.Lazy.Internal (ByteString (..))
import           Data.Text  (Text)
-- import           Data.Text.Encoding            (decodeUtf8)

-- | Define a test data type.
data MyJson = MyJson
  { name       :: Text
  , identifier :: Int
  } deriving stock (Eq, Show)

instance FromJSON MyJson where
  parseJSON (Object v) = MyJson <$> v .: "name" <*> v .: "identifier"
  parseJSON _          = fail "Expected an object"
