{-|

Module      : SubSeqs
Description : A collection of algorithms to generate sub-sequences
Copyright   : © Frank Jung, 2020
License     : GPL-3

This is a collection of different ways (with different performance) to
create <https://en.wikipedia.org/wiki/Subsequence subsquences>.

= Examples

@
subSeqs1 "abc" should be ["a","ab","abc","ac","b","bc","c"]
subSeqs2 "abc" should be ["a","ab","abc","ac","b","bc","c"]
subSeqs3 "abc" should be ["abc","ab","ac","a","bc","b","c",""]
subSeqs4 "abc" should be ["abc","ab","ac","a","bc","b","c",""]
@

-}

module SubSeqs (subSeqs1, subSeqs2, subSeqs3, subSeqs4) where

import           Control.Monad (filterM)

-- | From Pearls of Functional Algorithm Design.
subSeqs1 :: [a] -> [[a]]
subSeqs1 []  = [[]]
subSeqs1 [x] = [[x]]
subSeqs1 (x:xs) = [x] : map (x :) xss ++ xss
  where xss = subSeqs1 xs

-- | Alternative definition using just 'foldr' and 'map'.
-- For non-empty lists only.
subSeqs2 :: [a] -> [[a]]
subSeqs2 = foldr (\ x s -> [x] : map (x:) s ++ s) []

-- | Using list comprehension.
subSeqs3 :: [a] -> [[a]]
subSeqs3 []     = [[]]
subSeqs3 (x:xs) = [x : subseq | subseq <- subSeqs3 xs] ++ subSeqs3 xs

-- | Create a power-set using 'Control.Monad'.
--
-- <https://blog.ssanj.net/posts/2018-04-10-how-does-filterm-work-in-haskell.html This blog>
-- explains 'filterM'.
subSeqs4 :: [a] -> [[a]]
subSeqs4 = filterM (const [True, False])
