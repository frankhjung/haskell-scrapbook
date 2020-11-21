{-|

Module      : Permutation
Description : Some alternative permutation functions
Copyright   : © Frank Jung, 2020
License     : GPL-3

-}

module Permutation (perms1,inserts) where

perms1 :: [a] -> [[a]]
perms1 []     = [[]]
perms1 (x:xs) = [zs | ys <- perms1 xs, zs <- inserts x ys]

inserts :: a -> [a] -> [[a]]
inserts x []     = [[x]]
inserts x (y:ys) = (x:y:ys):map (y:) (inserts x ys)
