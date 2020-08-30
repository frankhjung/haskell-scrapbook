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

import           Data.List          (permutations)
import           System.Environment (getArgs)

-- | Find poly-divisor of input string.
findPolyDiv :: Int -> [Int]
findPolyDiv x = filterPolyMod (length (show x)) (perms x)

-- | Filter numbers modulo @n ... 1@.
filterPolyMod :: Int -> [Int] -> [Int]
filterPolyMod n = filter (isPolyMod n)

-- | Test number is modulo @n ... 1@.
isPolyMod :: Int -> Int -> Bool
isPolyMod n x = all (==0) (polyMod n x)
  where
    polyMod :: Int -> Int -> [Int]
    polyMod n' x'
      | n' == 1    = [0]
      | otherwise = x' `mod` n' : polyMod (n' - 1) (x' `div` 10)

-- | Permutations of argument.
perms :: Int -> [Int]
perms xs = map read (permutations (show xs))

-- | Search for poly-divisors of digits @1 .. 9@.
main :: IO ()
main = getArgs >>= mapM_ (print . findPolyDiv . read)
