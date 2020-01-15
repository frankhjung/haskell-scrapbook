#!/usr/bin/runhaskell

{-# LANGUAGE LambdaCase #-}

-- | Yahtzee test suite.
-- See http://h2.jaguarpaw.co.uk/posts/good-design-and-type-safety-in-yahtzee/

-- | Dice values [1..6]
type DiceVals   = [Integer]
-- | Keep the dice, or re-roll?
data DiceChoice = Keep Integer | Reroll

-- | Generate all rolls from a given state.
allRolls :: [DiceChoice] -> [DiceVals]
allRolls = mapM $ \case
  Reroll -> [1..6]
  Keep v -> [v]

-- | Test case
example =
  let diceVals = [ Reroll, Keep 4, Keep 4, Reroll, Reroll ]
  in mapM_ print $ allRolls diceVals

-- Run example
main :: IO ()
main = example

