{-# LANGUAGE DerivingStrategies #-}

{-|

Module      : Lower
Description : Data type to produce only lowercase alphabetic characters.
Copyright   : © Frank Jung, 2021
License     : GPL-3

Reference
<https://stackoverflow.com/questions/23616455/haskell-type-new-type-or-data-for-only-an-upper-case-char
type for uppercase characters>

-}

module Lower
    ( Lower      -- export type only, not constructor
    , mkLower    -- export the smart constructor
    ) where

import           Data.Char (isAlpha, isLower)

newtype Lower = Lower Char deriving stock (Read, Show)

mkLower :: Char -> Either String Lower
mkLower c
  | isLower c && isAlpha c = Right (Lower c)
  | otherwise              = Left "Not lowercase"

-- {-# LANGUAGE DataKinds #-}
-- import Data.Finite
-- import Data.Char
--
-- newtype Letter = Letter{ getLetterIndex :: Finite 26 }
--
-- toLowerChar :: Letter -> Char
-- toLowerChar = chr . (+ ord 'a') . fromIntegral . getFinite . getLetterIndex
--
-- fromLowerChar :: Char -> Maybe Letter
-- fromLowerChar = fmap Letter . packFinite . fromIntegral . subtract (ord 'a') . ord
