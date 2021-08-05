{-|

Module      : MyReverse
Description : Implement reverse using left and right folds
Copyright   : © Frank Jung, 2020
License     : GPL-3

Reverse a list using foldl and foldr.

-}

module MyReverse (myRevl, myRevr, myRevr2) where

import           Control.Arrow ((>>>))

-- | Reverse a list using foldl.
myRevl :: [a] -> [a]
myRevl = foldl (flip (:)) []

-- | Reverse a list using foldr.
myRevr :: [a] -> [a]
myRevr xs = foldr (\ x acc -> (x :) >>> acc) id xs []

-- | Reverse using recursion and reverse operation.
myRevr2 :: [a] -> [a]
myRevr2 = rev revOp []
  where
    rev _ acc []      = acc
    rev op acc (x:xs) = rev op (acc `op` x) xs

-- | Operation to place element at the beginning of a list.
revOp :: [a] -> a -> [a]
revOp xs x = x : xs
