#!/usr/bin/env runhaskell

{-|

Module      : Random
Description : Test random generators.
Copyright   : © Frank Jung, 2020
License     : GPL-3
Source      : Haskell Design Patterns by Ryan Lemmer

-}

module Main (main) where

import           System.Random (mkStdGen, randomRs)

dice :: (Int, Int)
dice = (1, 6)

seed :: Int
seed = 111111

-- | Simulate roll a fair 6-sided dice 5 times.
-- With 'seed' of @111111@ we expect @[4,6,5,3,2]@.
main :: IO ()
main = print $ take 5 (randomRs dice (mkStdGen seed))
