{-|

Module      : Colours
Description : Explore semigroup with a colours example
Copyright   : © Frank Jung, 2022
License     : GPL-3

From "Get Programming with Haskell" by Will Kurt,
Lesson 17. Design by composition—Semigroups and Monoids

-}

module Colours (
                 Colour(..) -- Sample Colours
               ) where

-- | Acceptable colours.
data Colour =
              Red
            | Yellow
            | Blue
            | Green
            | Purple
            | Orange
            | Brown deriving (Show,Eq)

-- | Example semigroup with a colours.
instance Semigroup Colour where
    (<>) a b  | a == b = a
              | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
              | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
              | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
              | otherwise = Brown
