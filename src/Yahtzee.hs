{-# LANGUAGE LambdaCase #-}

{-|

Module      : Yahtzee
Description : Code from "Good Design an Type Safety in Yahtzee"
Copyright   : © Frank Jung, 2019
License     : GPL-3

Yahtzee test suite from blog
<http://h2.jaguarpaw.co.uk/posts/good-design-and-type-safety-in-yahtzee Good Design and Type Safety in Yahtzee>

-}

module Yahtzee (DiceChoice (..), DiceVals, allRolls) where

-- | Dice values from 1 to 6.
type DiceVals = [Integer]

-- | Keep the dice, or re-roll?
data DiceChoice = Keep Integer | Reroll

-- | Generate all rolls from a given state.
allRolls :: [DiceChoice] -> [DiceVals]
allRolls = mapM $ \case
  Reroll -> [1..6]
  Keep v -> [v]
