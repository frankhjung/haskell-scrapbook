#!/usr/bin/env runhaskell

{-

Find polydivisible numbers such that we use digits from 1 to 9, and the
first n digits are modulo n.

I first read about puzzle in Matt Parkers book,
<https://www.penguin.com.au/books/things-to-make-and-do-in-the-fourth-dimension-9780141975863 Things to make and do in the fourth dimension>

== Method

 * permute a list of digits 1..9
 * convert to int and filter modulo (length)
 * drop last digit of int list by integer division of 10
 * repeat modulo (length)

== Tests

>>> $ stack exec polydivisors 123
[123,321]

>>> $ stack exec polydivisors 123456789
[381654729]

-}

module Main (main) where

import           PolyDivisors       (findPolyDiv)
import           System.Environment (getArgs)

-- | Search for poly-divisors of digits 1..9
main :: IO ()
main = getArgs >>= mapM_ (print . findPolyDiv . read)
