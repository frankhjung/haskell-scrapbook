{-|

Module      : Caesar
Description : Caesar cipher
Copyright   : © Frank Jung, 2022
License     : GPL-3

Code from <https://www.manning.com/books/haskell-bookcamp Haskell Bookcamp>
by Philipp Hagenlocher

This is my version of a simplified
<https://en.wikipedia.org/wiki/Caesar_cipher Caesar Cipher>.

-}

module Caesar ( caesar
              , isAscii
              , asciiRot
              ) where

import           Data.Char (chr, ord)

-- | Limited ASCII character set, includes all printable characters except
-- the space character.
isAscii :: Char -> Bool
isAscii c = 33 <= oc && oc <= 126
  where oc = ord c

-- | Rotate a ascii character.
-- 94 = 126 - 33 + 1
-- 33 is starting offset of printable characters (after space)
asciiRot :: Int -> Char -> Char
asciiRot n x
  | isAscii x = rotate
  | otherwise = x
  where rotate = chr $ (n + ord x - 33) `mod` 94 + 33

-- | Simple implementation of Caesar Cipher.
caesar :: Int -> String -> String
caesar n = map (asciiRot n)
