{-|

Module      : Permutation
Description : Some alternative permutation functions
Copyright   : © Frank Jung, 2020
License     : GPL-3

From the book
<http://www.cs.ox.ac.uk/publications/books/adwh Algorithm Design with Haskell by R. Bird and J. Gibbons>.

-}

module Permutation (inserts, perms1, perms2, perms3, picks) where

-- | Generate permutations using list comprehensions.
--
-- Example:
--
-- >>> perms1 "abc"
-- ["abc","bac","bca","acb","cab","cba"]
perms1 :: [a] -> [[a]]
perms1 []     = [[]]
perms1 (x:xs) = [zs | ys <- perms1 xs, zs <- inserts x ys]

-- | Insert character into list at each location.
--
-- Example:
--
--  >>> inserts 'a' "bc"
--  ["abc","bac","bca"]
inserts :: a -> [a] -> [[a]]
inserts x []     = [[x]]
inserts x (y:ys) = (x:y:ys) : map (y:) (inserts x ys)

-- | Generate permutations using list comprehensions.
--
-- Example:
--
-- >>> perms2 "abc"
-- ["abc","acb","bac","bca","cab","cba"]
perms2 :: [a] -> [[a]]
perms2 [] = [[]]
perms2 xs = concatMap subperms (picks xs)
  where subperms (y,ys) = map (y:) (perms2 ys)

-- | Pick each member from list return tuple of member and items remaining.
--
-- Example:
--
-- >>> picks "abc"
-- [('a',"bc"),('b',"ac"),('c',"ab")]
picks :: [a] -> [(a, [a])]
picks []     = []
picks (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- picks xs]


-- | Use fold to generate permutaions.
perms3 :: [a] -> [[a]]
perms3 = foldr (concatMap . inserts) [[]]
