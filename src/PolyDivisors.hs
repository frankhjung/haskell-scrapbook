{-|

Module      : PolyDivisors
Description : Find poly-divisors of a number
Copyright   : © Frank Jung, 2020
License     : GPL-3

Find poly-divisible numbers such that we use digits from 1 to 9, and the
first n digits are modulo n for all digits in the number.

I first read about puzzle in Matt Parkers book,
<https://www.penguin.com.au/books/things-to-make-and-do-in-the-fourth-dimension-9780141975863 Things to make and do in the fourth dimension>.

== Method

 * convert string input to int
 * permute a list of digits 1..9
 * filter modulo (length) for length digits

== Exploration

Investigate how to use a fold instead of recursion:

>>> x = 123456789
>>> n = length (show x)

>>> λ> map (\p -> x `div` 10^p ) [0..n-1]
[123456789,12345678,1234567,123456,12345,1234,123,12,1]

>>> λ> f x = let n = length (show x) in x `mod` n == 0
>>> λ> f 12
True

>>> λ> foldr (\x -> (&&) (f x)) True xs
False

== Examples

>>> $ stack exec polydivisors 123
[123,321]

>>> $ stack exec polydivisors 123456
[123654,321654]

>>> $ stack exec polydivisors 123456789
[381654729]

-}

module PolyDivisors (findPolyDiv
                    , isPolyMod
                    , isPolyMod'
                    , isPolyMod''
                    ) where

import           Data.List (permutations)

-- | Find poly-divisor of input string.
findPolyDiv :: Int -> [Int]
findPolyDiv = filter isPolyMod . perms

-- | Permutations of argument.
perms :: Int -> [Int]
perms xs = map read (permutations (show xs))

-- | @isPolyMod@: Test number is modulo @n ... 1@.
isPolyMod :: Int -> Bool
isPolyMod x = polyMod (length (show x)) x
  where
    -- Is value modulus of length?
    -- ie. Is 123 mod 3 = 0?
    isModLen :: Int -> Int -> Bool
    isModLen a n = let a' = a `div` 10 ^ n in a' `mod` length (show a') == 0
    -- fold version of recursive 'polyMod'
    polyMod :: Int -> Int -> Bool
    polyMod n a = foldr ((&&) . isModLen a) True [0..n-1]

-- | @isPolyMod'@: Test number is modulo @n ... 1@.
isPolyMod' :: Int -> Bool
isPolyMod' x = foldr ((&&) . isModLen) True xs
  where
     -- list of x's reduced by factor of 10 for length of x as a string
     -- eg. 123 gives [123, 12, 1]
     xs = map (\p -> x `div` 10^p ) [0..length (show x) - 1]
     -- Is value modulus of length?
     -- ie. Is 123 mod 3 = 0?
     isModLen :: Int -> Bool
     isModLen a = a `mod` length (show a) == 0

-- | @isPolyMod''@: Test number is modulo @n ... 1@.
isPolyMod'' :: Int -> Bool
isPolyMod'' x = all (== 0) (polyMod (length (show x)) x)
  where
    polyMod :: Int -> Int -> [Int]
    polyMod n a
      | n == 1    = [0]
      | otherwise = a `mod` n : polyMod (n - 1) (a `div` 10)
