#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}

{-|

Module      : Yahtzee
Description : Code from "Good Design an Type Safety in Yahtzee"
Copyright   : © Frank Jung, 2019
License     : GPL-3

Yahtzee test suite.

Call `main` to show example.

Source: <http://h2.jaguarpaw.co.uk/posts/good-design-and-type-safety-in-yahtzee/ Good Design and Type Safety in Yahtzee>

-}

module Yahtzee (DiceChoice (..), DiceVals, allRolls, main, example) where

-- | Dice values from 1 to 6.
type DiceVals = [Integer]

-- | Keep the dice, or re-roll?
data DiceChoice = Keep Integer | Reroll

-- | Generate all rolls from a given state.
allRolls :: [DiceChoice] -> [DiceVals]
allRolls = mapM $ \case
  Reroll -> [1..6]
  Keep v -> [v]

-- | An example test case.
example :: IO ()
example = let diceVals = [ Reroll, Keep 4, Keep 4, Reroll, Reroll ]
  in mapM_ print $ allRolls diceVals

-- | Run example:
--
-- >>> runhaskell yahtzee.hs
--
main :: IO ()
main = example
