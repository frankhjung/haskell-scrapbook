{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

Parse JSON when the first key is variable. Based of code from
<https://stackoverflow.com/questions/35756175/parse-json-with-aeson-when-first-key-is-variable>

- The `Generic` language extension is used to automatically derive the
  `FromJSON` instance for the `Thing` type. The `FromJSON` typeclass
  defines a way to parse JSON into a Haskell data type. A `FromJSON`
  instance is required to use the `eitherDecode` function.

- The test JSON string is a list of objects with more than one key. We need
  to tell the `FromJSON` parser that the first key is variable. We do this
  by defining an instance of the `FromJSON` typeclass using the `instance`
  keyword.

- JSON is represented as a ByteString because this is more efficient than
  Strings.

- We use the `deriving` keyword to derive the `Generic` instance for the
  `Thing` type. The `Generic` typeclass defines a way to convert a Haskell
  data type into a generic representation. This generic representation is
  used to implement the `FromJSON` typeclass.

- The JSON is represented as the `Thing` type. The type contains three
  fields: `version`, `items`, and `description`.

The output of the above program is:

@
Right (
  fromList [
    ("THING1",Thing {version = 1.0, items = ["T11"], description = "single"})
   ,("THING2",Thing {version = 2.4, items = ["T21","T22"], description = "paired"})
  ])
@

-}

module Main (main) where

import           Data.Aeson                    (FromJSON, eitherDecode)
import           Data.ByteString.Lazy.Internal (ByteString (..))
import           Data.Map                      (Map)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)

-- Record representing a single thing
data Thing =
  Thing {
    version     :: Float,
    items       :: [Text],
    description :: Text
  } deriving (Show, Generic)

-- | Parse JSON when the first key is variable.
instance FromJSON Thing

-- | JSON test string to parse.
jsonString :: ByteString
jsonString =
  "{\"THING1\": {\
   \ \"version\": 1.0,\
   \ \"items\": [\"T11\"],\
   \ \"description\": \"single\"\
   \ },\
   \\"THING2\": {\
   \ \"version\": 2.4,\
   \ \"items\": [\"T21\", \"T22\"],\
   \ \"description\": \"paired\"\
   \}}"

-- | Parse JSON when the first key is variable.
--
-- Same as:
--  case eitherDecode jsonString :: Either String (Map Text Thing) of
--    Left message -> print message
--    Right parsed -> print parsed
main :: IO ()
main = either print print (eitherDecode jsonString :: Either String (Map Text Thing))
