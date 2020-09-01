{-|

Module      : BinarySearch
Description : Example binary search
Copyright   : © Frank Jung, 2020
License     : GPL-3

Example implementation of
<https://en.wikipedia.org/wiki/Binary_search_algorithm binary search>.

-}

module BinarySearch (bsearch) where

-- import           Control.Lens (ix, (^?))
import           Data.Maybe (fromJust, listToMaybe)

-- | Binary Search
--
-- Example using an integer array, search list @[1..6]@ for values @[1..8]@.
--
-- Expect @1..6@ will return 'Just' values, while @7@ and @8@ will return
-- 'Nothing'.
--
-- The same also applies for strings. Try "abcdef" and "gh".
bsearch :: (Ord a) => [a] -> a -> Maybe a
bsearch [] _ = Nothing
bsearch xs key
    | key < fromJust val = bsearch (take mid xs) key
    | key > fromJust val = bsearch (drop (mid + 1) xs) key
    | otherwise = val
  where
    mid = length xs `div` 2
    val = listToMaybe (drop mid xs)
    -- val = xs ^? ix mid     -- from Control.Lens
