#!/usr/bin/env runhaskell

{-

Count words from file read from STDIN.

Word Count as per <https://linux.die.net/man/1/wc wc(1)>.

>>> wc -w wordcount.hs
38 wordcount.hs

>>> cat wordcount.hs | runhaskell wordcount.hs
38

-}

module Main (main) where

-- | Run example, read values from STDIN.
--
-- >>> cat wordcount.hs | runhaskell wordcount.hs
main :: IO ()
main = interact $ show . length . words
