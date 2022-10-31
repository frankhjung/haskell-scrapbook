{-|

Module      : PolyDivisors
Description : Find poly-divisors of a number
Copyright   : © Frank Jung, 2020
License     : GPL-3

Find poly-divisible numbers such that we use digits from 1 to 9, and the
first n digits are modulo n for all digits in the number.

I first read about puzzle in Matt Parkers book,
<https://www.penguin.com.au/books/things-to-make-and-do-in-the-fourth-dimension-9780141975863 Things to make and do in the fourth dimension>.

The poly divisors listed here are in descending order of performance.
i.e. @isPolyMod@ better than @isPolyMod'@ which is better than @isPolyMod''@

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

Load in GHCi:

>>> $ cabal repl
>>> λ> :load PolyDivisors

>>> λ> findPolyDiv 123
[123,321]

>>> λ> findPolyDiv 123456
[123654,321654]

>>> λ> findPolyDiv 123456789
[381654729]

-}

module PolyDivisors ( findPolyDiv
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

-- | 'isPolyMod': Test number is modulo @n ... 1@.
isPolyMod :: Int -> Bool
isPolyMod x = foldr ((&&) . isModLen) True xs
  where
    -- number of digits in input
    n = length (show x)
    -- list of x's reduced by factor of 10 for length of x as a string
    -- eg. 123 gives [(123, 3), (12, 2), (1,1)]
    xs = map (\p -> (x `div` 10 ^ p, n - p) ) [0..n - 1]
    -- Check tuple (x, n) is modulus 0
    -- ie. Is x mod n = 0?
    isModLen :: (Int, Int) -> Bool
    isModLen xn = uncurry mod xn == 0

-- | @isPolyMod''@: Test number is modulo @n ... 1@.
isPolyMod' :: Int -> Bool
isPolyMod' x = all (== 0) (polyMod n x)
  where
    -- number of digits in input
    n = length (show x)
    -- recurse reducting digits of list
    polyMod :: Int -> Int -> [Int]
    polyMod m a
      | m == 1    = [0]  -- as m is from length this is always positive
      | otherwise = a `mod` m : polyMod (m - 1) (a `div` 10)

-- | @isPolyMod''@: Test number is modulo @n ... 1@.
isPolyMod'' :: Int -> Bool
isPolyMod'' x = all (== 0) xs
  where
    -- number of digits in input
    n = length (show x)
    -- list of x's reduced by factor of 10 for length of x as a string
    -- eg. 123 gives [(123 % 3), (12 % 2), (1 % 1)]
    xs = map (\p -> x `div` 10 ^ p `mod` (n - p) ) [0..n - 1]
