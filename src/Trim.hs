{-|

Module      : Trim
Description : An inefficient trim white space from front of string.
Copyright   : © Frank Jung, 2020
License     : GPL-3

An inefficient alternative to 'dropWhile'.

= Examples

>>> trim " hello world "
"hello world"

>>> dropWhile' isSpace " hello world "
"hello word "

-}

module Trim (dropWhile', trim) where

import           Data.Char (isSpace)
import           Data.List (dropWhileEnd, foldl')

-- | Trim white space from start and end of string.
trim :: String -> String
trim = trimStart . trimEnd

-- | Trim white space from start of string.
trimStart :: String -> String
trimStart = dropWhile isSpace

-- | Trim white space from end of string.
trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

-- | Drop from list while predicate true.
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p = reverse . foldl' (\z x -> if null z && p x then z else x:z) []

