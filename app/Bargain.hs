{-# LANGUAGE RecordWildCards #-}

-- module Main (main) where
module Main ( main
            , Name
            , Item
            , Color
            , Assignment
            , Solution
            , names
            , items
            , colors
            , savings
            , answers ) where

{-

== Solve the Bargain logic puzzle (easy)

Benny, Carla and Danni each picked up a bargain in the January sales
with all three items having a reasonable saving on the original selling
price - the biggest being $11.50. From this information and the following
clues, for each buyer, can you determine the item they bought, the color
and the amount saved on the original selling price?

=== Clues

1. The blue jacket was not bought by Benny
2. Danni was pleased to have $9.00 knocked off the item she bought
3. The red item had more knocked off than the umbrella
4. The trousers did not have $5.00 knocked off and were not bought by Benny or Carla

=== Answer

+-------+----------+-------+---------+
| Name  | Item     | Color | Savings |
+=======+==========+=======+=========+
| Benny | Umbrella | Green |    5.0  |
| Carla | Jacket   | Blue  |   11.5  |
| Danni | Trousers | Red   |    9.0  |
+-------+----------+-------+---------+

=== References

- https://www.ahapuzzles.com/logic/logic-puzzles/a-bargain/
- https://www.youtube.com/watch?v=bPyR1ttdE7o&t=1623s&ab_channel=Tweag

-}

import           Control.Monad (guard)
import           Data.List     (find, permutations, zipWith4)

data Name = Benny | Carla | Danni
              deriving (Enum, Bounded, Show, Eq)

data Item = Trousers | Jacket | Umbrella
              deriving (Enum, Bounded, Show, Eq)

data Color = Red | Green | Blue
              deriving (Enum, Bounded, Show, Eq)

names = [minBound..maxBound]
items = [minBound.. maxBound]
colors = [minBound.. maxBound]
savings = [5.0, 9.0, 11.5]

data Assignment = MkAssignment {
                    name   :: Name
                  , item   :: Item
                  , color  :: Color
                  , saving :: Float }
                    deriving (Eq)

instance Show Assignment where
  show MkAssignment {..} = "("
                            ++ show name ++ ","
                            ++ show item ++ ","
                            ++ show color ++ ","
                            ++ show saving ++ ")"

type Solution = [Assignment]

answers :: [Solution]
answers = do
  solution@[benny, carla, danni]
    <- [zipWith4 MkAssignment names is cs ss
       | is <- permutations items
       , cs <- permutations colors
       , ss <- permutations savings]                    -- (216)

  -- Clue 1) The blue jacket was not bought by Benny
  Just bluejackets <- [find (\a -> color a == Blue && item a == Jacket) solution]
  guard ( name bluejackets /= Benny )                   -- (_48)

  -- Clue 2) Danni was pleased to have $9.00 knocked off the item she bought
  guard (saving danni == 9.0)                           -- (_16)

  -- Clue 3) The red item had more knocked off than the umbrella
  Just reditem <- [find (\a -> color a == Red) solution]
  Just umbrella <- [find (\a -> item a == Umbrella) solution]
  guard (saving reditem > saving umbrella)              -- (__4)

  -- Clue 4) The trousers did not have $5.00 knocked off and were not bought by
  -- Benny or Carla
  Just trousers <- [find (\a -> item a == Trousers) solution]
  let clue4 i = saving i /= 5.0 && name i `notElem` [Benny, Carla]
  guard (clue4 trousers)                                -- (__1) ** Unique

  return solution

-- | Solve the logic puzzle.
main :: IO ()
main = mapM_ print answers
