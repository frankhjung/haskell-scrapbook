{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE OverloadedRecordDot #-} -- supported in 9.2

module Main (main) where

{-

module Main ( main
            , Player (..)
            , Team (..)
            , Assignment (..)
            , Solution
            , players
            , teams
            , jerseys
            , goals
            , answers
            ) where

== Solve the Hockey Time logic puzzle (medium)

This year's dream team has been picked! Players from around the nation have
been chosen to participate in a final playoff extravaganza. Your task is to
help figure out 5 of the players who were chosen to participate, the team
they play for, how many goals they have scored this season and the number
on their jersey.

=== Clues

1. Mahoney is either the person who wears number 26 or the player from the Icers
2. Quill doesn't wear number 26
3. Nates scored 2 goals more than Pilgrim
4. The player from the Red Boots scored 1 goal more than the person from the Warriors
5. The player who wears number 25 plays for the Warriors.
6. The player who wears number 4 scored 1 goal less than the player who wears number 26.
7. The person with 8 goals is from the Warriors
8. Douglas scored 1 goal more than the player who wears number 25
9. Of the player from the Cougars and the player with 11 goals,
   one is Quill and the other wears number 22.

=== References

* https://www.ahapuzzles.com/logic/logic-puzzles/hockey-time/
* https://www.youtube.com/watch?v=bPyR1ttdE7o&t=1623s&ab_channel=Tweag
* https://github.com/goldfirere/video-resources/blob/main/2022-08-12-java/Haskell.hs

-}

import           Control.Monad (guard)
import           Data.List     (find, permutations, zipWith4)

data Player = Douglas | Mahoney | Nates | Pilgrim | Quill
              deriving (Enum, Bounded, Show, Eq)

data Team = Cougars | Icers | Monsters | RedBoots | Warriors
              deriving (Enum, Bounded, Show, Eq)

players :: [Player]
players = [minBound..maxBound]
teams :: [Team]
teams = [minBound.. maxBound]
jerseys :: [Int]
jerseys = [4, 7, 22, 25, 26]
goals :: [Int]
goals = [8, 9, 10, 11, 12]

data Assignment = MkAssignment {
                    player :: Player
                  , team   :: Team
                  , jersey :: Int
                  , goal   :: Int }
                    deriving (Eq)

instance Show Assignment where
  show MkAssignment {..} = "("
                            ++ show goal ++ ", "
                            ++ show player ++ ", "
                            ++ show team ++ ", "
                            ++ show jersey ++ ")"

type Solution = [Assignment]

answers :: [Solution]
answers = do
  solution@[douglas, mahoney, nates, pilgrim, quill]
    <- [zipWith4 MkAssignment players ts js gs
       | ts <- permutations teams
       , js <- permutations jerseys
       , gs <- permutations goals]                              -- (1728000)

  -- Clue (1) Mahoney is either the player who wears number 26
  --          or the player from the Icers
  guard (jersey mahoney == 26 || team mahoney == Icers)
  guard (not (jersey mahoney == 26 && team mahoney == Icers))   -- ( 552960)

  -- Clue (2) Quill doesn't wear number 26
  guard (jersey quill /= 26)                                    -- ( 483840)
  -- (from clue 5)
  -- guard (jersey quill /= 25)

  -- Clue (3) Nates scored 2 goals more than Pilgrim
  guard (goal nates == goal pilgrim + 2)                        -- (  72576)

  -- Clue (4) The player from the Red Boots scored 1 goal more
  --          than the player from the Warriors
  Just redboots <- [find (\p -> team p == RedBoots) solution]
  Just warriors <- [find (\p -> team p == Warriors) solution]
  guard (goal redboots == goal warriors + 1)                    -- (  14592)
  -- Player in warriors scored 8 goals (from clue 7)
  -- guard (goal redboots == 9)

  -- Clue (5) The player who wears number 25 plays for the Warriors.
  guard (jersey warriors == 25)                                 -- (   2760)

  -- Clue (6) The player who wears number 4 scored 1 goal less than
  --          the player who wears number 26
  Just jersey4 <- [find (\p -> jersey p == 4) solution]
  Just jersey26 <- [find (\p -> jersey p == 26) solution]
  guard (goal jersey4 + 1 == goal jersey26)                     -- (    464)

  -- Clue (7) The player with 8 goals is from the Warriors
  guard (goal warriors == 8)                                    -- (    144)
  -- means player from red boots has 9 goals (clue 4)
  -- guard (goal redboots == 9)

  -- Clue (8) Douglas scored 1 goal more than the player who wears number 25
  -- Jersey 25 is in the Warriors (from clue 5)
  -- Person in Warriors has 8 goals (from clue 7)
  guard (goal douglas == 9)                                     -- (     40)
  -- (from clue 4)
  -- guard (team douglas == RedBoots)

  -- Clue (9) Of the person from the Cougars and the person with 11 goals,
  -- one is Quill and the other wears number 22
  Just jersey22 <- [find (\p -> jersey p == 22) solution]
  let clue9 p1 p2 = team p1 == Cougars && goal p2 == 11
  guard (clue9 quill jersey22 || clue9 jersey22 quill)
  guard (player quill /= player jersey22) -- can not be the same player

  -- Just cougars <- [find (\p -> team p == Cougars) solution]
  -- Just goals11 <- [find (\p -> goal p == 11) solution]
  -- let clue9 p1 p2 = player p1 == Quill && jersey p2 == 22
  -- guard (clue9 cougars goals11 || clue9 goals11 cougars)
  -- guard (player cougars /= player goals11)

  return solution

-- | Solve the Hockey Time logic puzzle.
main :: IO ()
main = mapM_ print answers
