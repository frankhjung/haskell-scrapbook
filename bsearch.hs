#!/usr/bin/env runhaskell

{-|

Module      : BinarySearch
Description : Example binary search.
Copyright   : © Frank Jung, 2020
License     : GPL-3

Example implementation of binary search.

-}

module BinarySearch (main, bsearch) where

-- import           Control.Lens (ix, (^?))
import           Data.Maybe (Maybe (..), fromJust, listToMaybe)

-- | Binary Search
bsearch :: (Ord a) => [a] -> a -> Maybe a
bsearch [] _ = Nothing
bsearch xs key
    | key < fromJust val = bsearch (take mid xs) key
    | key > fromJust val = bsearch (drop (mid + 1) xs) key
    | otherwise = val
  where
    mid = length xs `div` 2
    val = listToMaybe (drop mid xs)
    -- val = xs ^? ix mid

-- | Run example using an integer array.
--
-- Search list [1..6] for values [1..8].
--
-- Expect 7 and 9 to return, Nothing.
--
-- The same will apply for strings:
--
-- "abcdef" and "gh".
main :: IO ()
main = print $ map (bsearch xs) (xs <> [7,8])
  where xs = [1..6] :: [Int]

