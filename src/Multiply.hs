{-|

Module      : Multiply
Description : Multiplication alogorithms
Copyright   : © Frank Jung, 2021
License     : GPL-3

Haskell implementations of mulitplication alogorithms as described by
<https://play.google.com/store/books/details?pcampaignid=books_read_action&id=UqxYBQAAQBAJ From Mathematics to Generic Programming>.

-}

module Multiply ( multiply0
                , multiply1
                , multiply2
                , double
                , half
                , halfs
                ) where

-- | The <https://en.wikipedia.org/wiki/Ancient_Egyptian_multiplication Egyptian multiplication>
-- as described by Ahmes.
multiply0 :: Int -> Int -> Int
multiply0 n a = if n == 1 then a else a + multiply0 (n - 1) a

-- | Improved Ahmes algorithm.
multiply1 :: Int -> Int -> Int
multiply1 n a
  | n == 1    = a
  | odd n     = r + a
  | otherwise = r
  where r = multiply1 (half n) (double a)

-- | Improved Ahmes algorithm using an accumulator.
multiply2 :: Int -> Int -> Int
multiply2 n a
  | n == 1    = a
  | otherwise = multiplyacc a (n - 1) a
  where
    multiplyacc :: Int -> Int -> Int -> Int
    multiplyacc r m b
      | m == 1    = r + b
      | odd m     = multiplyacc (r + b) (half m) (double b)
      | otherwise = multiplyacc r (half m) (double b)

-- | Double the current value.
double :: Int -> Int
double a = a + a

-- | Half the current value.
half :: Int -> Int
half = flip div 2

-- | List of halves until 1.
halfs :: Int -> [Int]
halfs n = takeWhile (>0) (iterate half n)

