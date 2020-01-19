#!/usr/bin/env runhaskell

{-|
Module      : SubSequences
Description : A collection of algorithms to generate sub-sequences.
Copyright   : © Frank Jung, 2020
License     : GPL-3
-}

module SubSequence (subseqs1, subseqs2, subseqs3, subseqs4, main) where

import           Control.Monad (filterM)
import           Data.List     (subsequences)

-- | From Pearls of Functional Algorithm Design.
subseqs1 :: [a] -> [[a]]
subseqs1 []  = [[]]
subseqs1 [x] = [[x]]
subseqs1 (x:xs) = [x] : map (x :) xss ++ xss
  where xss = subseqs1 xs

-- | Alternative definition using just @foldr@ and @map@.
subseqs2 :: [a] -> [[a]]
subseqs2 = foldr (\ x s -> [x] : map (x:) s ++ s) []

-- | Using list comprehension.
subseqs3 :: [a] -> [[a]]
subseqs3 []     = [[]]
subseqs3 (x:xs) = [x : subseq | subseq <- subseqs3 xs] ++ subseqs3 xs

-- | Create a power-set using @Control.Monad@.
subseqs4 :: [a] -> [[a]]
subseqs4 = filterM (const [True, False])

-- | Show examples including use of @subsequences@
main :: IO ()
main = mapM_ (print . ($ "abc")) [subseqs1, subseqs2, subseqs3, subseqs4, subsequences]
