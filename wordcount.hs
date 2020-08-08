#!/usr/bin/env runhaskell

{-|

Module      : WordCount
Description : Count words from file read from STDIN.
Copyright   : © Frank Jung, 2019
License     : GPL-3

Word Count as per <https://linux.die.net/man/1/wc wc(1)>.

>>> wc -w wordcount.hs
38 wordcount.hs

>>> cat wordcount.hs | runhaskell wordcount.hs
38

-}

module WordCount (main) where

-- | Count words from file read from STDIN.
main :: IO ()
main = interact $ show . length . words
