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
myRevr as = foldr (\ a s -> (a :) >>> s) id as []

-- | Second version to reverse a list using foldr.
myRevr2 :: [a] -> [a]
myRevr2 = rev revOp []
  where
    rev _ acc []      = acc
    rev op acc (x:xs) = rev op (acc `op` x) xs

-- | Reverse operation.
revOp :: [a] -> a -> [a]
revOp xs x = x : xs
