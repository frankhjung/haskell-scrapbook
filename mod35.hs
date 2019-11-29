#!/usr/bin/runhaskell

{-|

Module      : Mod35
Description : Test if modulus 3 or 5
Copyright   : © Frank Jung, 2019
License     : GPL-3

Tests whether value a integer value is modulus 3 or modulus 5.

-}

module Mod35 (mod35,main) where

import           System.Environment (getArgs)

-- | Test if modulus 3 or 5.
mod35 :: Int      -- ^ value to test
         -> Bool  -- ^ true if modulus 3 and/or 5
mod35 n = (n `mod` 3 == 0) || (n `mod` 5 == 0)

-- | To run, call:
--
-- >>> runhaskell mod35.hs n
--
-- Below is an alternative `main` where value read from STDIN.
--
-- @
-- main :: IO ()
-- main = interact $ show . mod35 . read . head . words
-- @
--
-- To run, call:
--
-- >>> echo n | runhaskell mod35.hs
main :: IO ()
main = getArgs >>= \args -> print $ (mod35 . read . head) args
