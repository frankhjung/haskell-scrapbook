{-|

Module      : Qsort
Description : A simple implementation of Qsort.
Copyright   : © Frank Jung, 2019
License     : GPL-3

A simple implementation of <https://en.wikipedia.org/wiki/Qsort Qsort>.

From "Programming in Haskell" by Graham Hutton.

>>> qsort [3,5,1,2,4,2]
[1,2,2,3,4,5]

If @a <= x@ is replaced with @a < x@, then only unique values are reported.

>>> qsort' [3,5,1,2,4,2]
[1,2,3,4,5]

To reverse sort, switch smaller and larger in 'qsort'.

>>> qsort'' [3,5,1,2,4,2]
[5,4,3,2,2,1]

-}

module Qsort (qsort) where

-- | A simple implementation of <https://en.wikipedia.org/wiki/Qsort Qsort>.
qsort :: (Show a, Ord a)
  => [a] -- ^ list to sort
  -> [a] -- ^ the sorted list
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]
