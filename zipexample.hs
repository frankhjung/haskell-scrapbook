#!/usr/bin/runhaskell

{-|

Module      : ZipExample
Description : Examples with zipwith
Copyright   : © Frank Jung, 2020
License     : GPL-3

Split a list in half using zipWith.

Source: <https://github.com/quchen/articles/blob/master/2018-11-22_zipWith_const.md My favorite Haskell function by David Luposchainsky>

-}

module ZipExample (every2nd, zipOverflow, splitMiddle, main) where

-- | Drop every second element.
every2nd :: [a] -> [a]
every2nd (x:_:xs) = x : every2nd xs
every2nd _        = []

-- | Ignore count of first elements then print overflow from second.
zipOverflow :: [a] -> [a] -> [a]
zipOverflow (_:xs) (_:ys) = zipOverflow xs ys
zipOverflow [] ys         = ys -- end of first list so return rest of second
zipOverflow xs []         = xs -- end of second list so return rest of first

-- | Split list into two.
splitMiddle :: [a] -> ([a], [a])
splitMiddle xs = let firstHalf = zipWith const xs (every2nd xs)
                     secondHalf = zipOverflow firstHalf xs
                 in (firstHalf, secondHalf)

-- | To run, call:
--
-- >>> echo helloworld | runhaskell zipexample.hs
--
main :: IO ()
main = interact $ show . splitMiddle . head . lines

