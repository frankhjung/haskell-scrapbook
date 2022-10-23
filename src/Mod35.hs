{-|

Module      : Mod35
Description : Test if modulus 3 or 5
Copyright   : © Frank Jung, 2019
License     : GPL-3

Tests whether value a integer value is modulus 3 or modulus 5.

>>> mod35 15

-}

module Mod35 (mod35) where

-- | Test if modulus 3 or 5.
mod35 :: Int      -- ^ value to test
         -> Bool  -- ^ true if modulus 3 and/or 5
mod35 n = n `mod` 3 == 0 || n `mod` 5 == 0
