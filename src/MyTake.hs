{- |

Module      : MyTake
Description : Implement take
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module MyTake (myTake) where

-- | My version of take.
-- This does not check for negative numbers.
--
-- >>> myTake 1 "abc"
-- "a"
myTake :: Int -> [a] -> [a]
myTake n _      | n <= 0 = []
myTake _ []              = []
myTake n (x:xs)          = x : myTake (pred n) xs
