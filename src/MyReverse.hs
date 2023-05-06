{-|

Module      : MyReverse
Description : Implement reverse using left and right folds
Copyright   : © Frank Jung, 2020
License     : GPL-3

Reverse a list using foldl and foldr.

-}

module MyReverse (myRevl, myRevr, myRevRec) where

import           Control.Arrow ((>>>))

-- | Reverse a list using foldl.
myRevl :: [a] -> [a]
myRevl = foldl revOp []

-- | Reverse a list using foldr.
-- This is the best performing version.
-- Same as:
-- @
-- myRevr xs = foldr (\ x acc -> (x :) >>> acc) id xs []
-- @
myRevr :: [a] -> [a]
myRevr = flip (foldr ((>>>) . (:)) id) []

-- | Reverse using recursion and reverse operation.
myRevRec :: [a] -> [a]
myRevRec = rev revOp []
  where
    rev _ acc []      = acc
    rev op acc (x:xs) = rev op (acc `op` x) xs

-- | Operation to place element at the beginning of a list.
-- Same as:
-- @
-- revOp xs x = x : xs
-- @
revOp :: [a] -> a -> [a]
revOp = flip (:)
