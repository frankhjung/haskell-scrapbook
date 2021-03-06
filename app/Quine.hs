#!/usr/bin/env runhaskell

{-

Quine: a program that takes no input and produces a copy
of itself as output.

Word Count as per <https://en.wikipedia.org/wiki/Quine_(computing) Quine>

-}

module Main (main) where

-- | Produce thyself!
main :: IO ()
main = putStr (quine q)
quine :: String -> String
quine s = s ++ show s
q :: String
q = "main = putStr (quine q)\nquine s = s ++ show s\nq = "
