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

module Lower (mkLower) where

import           Data.Char (isAlpha, isLower)

-- | Make a lowercase letter.
mkLower :: Char -> Either String Char
mkLower x
  | isLower x && isAlpha x = Right x
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
