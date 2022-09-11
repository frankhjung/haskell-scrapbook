{-# LANGUAGE RecordWildCards #-}

module Main (main) where

{-

module Main ( main
            , First
            , Last
            , Road
            , Assignment
            , Solution
            , firsts
            , lasts
            , values
            , roads
            , answers ) where

== Expensive Homes Logic Puzzle

Three members of our society live in 1 million plus homes in one of the
city's more affluent suburbs. From the clues, for each person, can you
determine their surname, the street in which they live and the value of
their home?

=== Clues

1. Kassandra's home is worth more than that of the person whose last name
   is Holloway
2. The Elm Tree Road resident owns the least valued home.
3. Alexandra lives in neither Treelined Boulevard nor Elm Tree Road but in
   the most valued home.
4. Holt is not the last name of the Wattle Grove resident.

=== Answer

+------------+----------+-------------+----------+
| First      | Last     | House Value | Road     |
+============+==========+=============+==========+
| Alexandra  | Melton   | $1,499,000  | Wattle   |
| Angelo     | Holloway | $1,050,000  | Elm      |
| Kassandra  | Holt     | $1,278,500  | Treelines|
+------------+----------+-------------+----------+

=== References

* <https://www.ahapuzzles.com/logic/logic-puzzles/expensive-homes/>

-}

import           Control.Monad (guard)
import           Data.List     (find, permutations, zipWith4)

data First = Alexandra | Angelo | Kassandra
              deriving (Enum, Bounded, Show, Eq)

data Last = Holloway | Holt | Melton
              deriving (Enum, Bounded, Show, Eq)

data Road = Elm | Treelines | Wattle
              deriving (Enum, Bounded, Show, Eq)

firsts :: [First]
firsts = [minBound..maxBound]
lasts :: [Last]
lasts = [minBound.. maxBound]
roads :: [Road]
roads = [minBound.. maxBound]
values :: [Int]
values = [1050000, 1278500, 1499000]


data Assignment = MkAssignment {
                    _first :: First
                  , _last  :: Last
                  , _value :: Int
                  , _road  :: Road }
                    deriving (Eq)

instance Show Assignment where
  show MkAssignment {..} = "("
                            ++ show _first ++ ","
                            ++ show _last ++ ","
                            ++ show _value ++ ","
                            ++ show _road ++ ")"

type Solution = [Assignment]

answers :: [Solution]
answers = do
  solution@[alexandra, _, kassandra]    -- angelo is unused
    <- [zipWith4 MkAssignment firsts ls vs rs
       | ls <- permutations lasts
       , vs <- permutations values
       , rs <- permutations roads]                                -- (216)

  -- Kassandra's home is worth more than that of the person whose last name
  -- is Holloway
  Just holloway <- [find (\p -> _last p == Holloway) solution]
  guard (_value kassandra > _value holloway)                      -- ( 72)

  -- The Elm Tree Road resident owns the least valued home.
  Just elm <- [find (\p -> _road p == Elm) solution]
  guard (_value elm == minimum values)                            -- ( 24)

  -- Alexandra lives in neither Treelined Boulevard nor Elm Tree Road but
  -- in the most valued home.
  guard (_road alexandra `notElem` [Treelines, Elm]
          && _value alexandra == maximum values)                  -- (  2)

  -- Holt is not the last name of the Wattle Grove resident.
  Just holt <- [find (\p -> _last p == Holt) solution]
  guard (_road holt /= Wattle)                                    -- unique

  return solution

-- | Solve the logic puzzle.
main :: IO ()
main = mapM_ print answers
