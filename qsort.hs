#!/usr/bin/env runhaskell

{-|

Module      : Qsort
Description : A simple implementation of Qsort.
Copyright   : © Frank Jung, 2019
License     : GPL-3

A simple implementation of <https://en.wikipedia.org/wiki/Qsort Qsort>.

From "Programming in Haskell" by Graham Hutton.

>>> echo -e "3 5 1 2 4 2 \n" | runhaskell qsort.hs
1 2 2 3 4 5

If @a <= x@ is replaced with @a < x@, then only unique values are reported.

>>> echo -e "1 3 5 1 2 4 2 \n" | runhaskell qsort.hs
1 2 3 4 5

To reverse sort, switch smaller and larger in qsort.

-}

module QSort (qsort, main) where

-- | A simple implementation of <https://en.wikipedia.org/wiki/Qsort Qsort>.
qsort :: (Show a, Ord a)
  => [a] -- ^ list to sort
  -> [a] -- ^ the sorted list
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]

-- | Sort contents from STDIN.
main :: IO ()
main = interact $ unwords . qsort . words

