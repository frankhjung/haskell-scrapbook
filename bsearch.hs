#!/usr/bin/env runhaskell

{-|

Module      : BinarySearch
Description : Example binary search.
Copyright   : © Frank Jung, 2019-2020
License     : GPL-3

Example implementation of binary search.

-}

module BinarySearch (main, bsearch) where

import           Control.Lens (ix, (^?))
import           Data.Maybe   (Maybe (..), fromJust)

-- | Binary Search
bsearch :: [Int] -> Int -> Maybe Int
bsearch [] _ = Nothing
bsearch xs key
    | key < fromJust val = bsearch (take mid xs) key
    | key > fromJust val = bsearch (drop (mid + 1) xs) key
    | otherwise = val
  where
    mid = length xs `div` 2
    val = xs ^? ix mid

-- | Run example using set integer array [1,..,6]
--
main :: IO ()
main = print $ map (bsearch xs) xs
  where xs = [1..6]

