{-|

Module      : Multiply
Description : Multiplication algorithms
Copyright   : © Frank Jung, 2021
License     : GPL-3

Haskell implementations of multiplication algorithms as described by
<https://play.google.com/store/books/details?pcampaignid=books_read_action&id=UqxYBQAAQBAJ From Mathematics to Generic Programming>.

-}

module Multiply ( multiply0
                , multiply1
                , multiply2
                , multiply3
                , multiply4
                , binary
                , double
                , doubles
                , half
                , halves
                ) where

-- | The <https://en.wikipedia.org/wiki/Ancient_Egyptian_multiplication Egyptian multiplication>
-- as described by Ahmes.
multiply0 :: Word -> Word -> Word
multiply0 a b = if a <= 1 then b else b + multiply0 (a - 1) b

-- | Improved Ahmes algorithm.
multiply1 :: Word -> Word -> Word
multiply1 a b
  | a <= 1    = b
  | odd a     = s + b
  | otherwise = s
  where s = multiply1 (half a) (double b)

-- | Improved Ahmes algorithm using an accumulator.
multiply2 :: Word -> Word -> Word
multiply2 a b
  | a <= 1    = b
  | otherwise = multiplyacc b (a - 1) b
  where
    multiplyacc :: Word -> Word -> Word -> Word
    multiplyacc s x y
      | x <= 1    = s + y
      | odd x     = multiplyacc (s + y) (half x) (double y)
      | otherwise = multiplyacc s (half x) (double y)

-- | Non-recursive version of Egyptian multiplication.
-- Based on
-- <http://www.mathnstuff.com/math/spoken/here/2class/60/egyptm.htm MathnStuff Egyptian multiplication>.
multiply3 :: Word -> Word -> Word
multiply3 a b = foldr ((+) . snd) 0 $ filter (odd . fst) $ zip (halves a) (doubles b)

-- | Non-recursive version of Egyptian multiplication
-- by <https://mathspp.com/blog/egyptian-multiplication#comment-5257985406 Rodrigo Girão Serrão>
multiply4 :: Word -> Word -> Word
multiply4 a b = foldl (\s p -> s + uncurry (*) p) 0 pairs
  where pairs = zip (binary a) (doubles b)

-- | Double the current value.
double :: Word -> Word
double a = a + a

-- | Continuously double value.
doubles :: Word -> [Word]
doubles = iterate double

-- | Half the current value.
half :: Word -> Word
half = flip div 2

-- | List of halves until 1.
halves :: Word -> [Word]
halves a = takeWhile (>0) (iterate half a)

-- | Repeatadly half the given integer until 1
-- then replace even entries with 0 and odd entries with 1.
binary :: Word -> [Word]
binary a = flip mod 2 <$> halves a
