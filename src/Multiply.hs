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
                ) where

-- | The "Egyptian multiplication" as described by Ahmes.
multiply0 :: Int -> Int -> Int
multiply0 n a = if n == 1 then a else a + multiply0 (n - 1) a

-- | Improved Ahmes algorithm.
multiply1 :: Int -> Int -> Int
multiply1 n a
  | n == 1    = a
  | odd n     = r + a
  | otherwise = r
  where r = multiply1 (n `div` 2) (a + a)

-- | Improved Ahmes algorithm.
multiply2 :: Int -> Int -> Int
multiply2 n a
  | n == 1    = a
  | otherwise = multiplyacc a (n - 1) a

-- | Improved algorithm which uses an accumulator.
multiplyacc :: Int -> Int -> Int -> Int
multiplyacc r n a
  | n == 1    = r + a
  | odd n     = multiplyacc (r + a) (n `div` 2) (a + a)
  | otherwise = multiplyacc r (n `div` 2) (a + a)

