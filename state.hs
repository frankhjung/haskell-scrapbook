#!/usr/bin/env runhaskell

{-|

Module      : State
Description : Deriving the State Monad
Copyright   : © Frank Jung, 2020
License     : GPL-3

From <https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html Deriving the State Monad by William Yaoh>

>>> echo -e "1 2 3 4 5 6 \n" | runhaskell state.hs
(2,["1","2","3","4","5","6"])

-}

module State (main) where

-- | Reverse a list, and increase a count of function calls.
reverseWithCount :: Int -> [a] -> (Int, [a])
reverseWithCount count list = (count + 1, reverse list)

-- | Reverse the list twice giving back original but incremented count.
reverseTwiceWithCount :: Int -> [a] -> (Int, [a])
reverseTwiceWithCount count list = (c', a')
  where
    (c, a) = reverseWithCount count list
    (c', a') = reverseWithCount c a

-- Reverse list of contents from command line.
main :: IO ()
main = interact $ show . reverseTwiceWithCount 0 . words
