{-# LANGUAGE DeriveAnyClass #-}

{-|

Module      : Compass
Description : Explore type classes
Copyright   : © Frank Jung, 2022
License     : GPL-3

From "Haskell in Depth" by Vitaly Bragilevsky,
Chapter 2. Manipulating a radar antenna with type classes.

-}

module Compass (
                  Direction(..) -- Compass direction
                , Turn(..) -- Turn instruction
                , every
                , cpred
                , csucc
                , rotate
                , rotateMany
                , rotateManyTurns
                , orientate
                , orientateMany
               ) where

-- | Cyclic bounded enumeration of compass directions.
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  -- | Predecessor of a Cyclic enumeration.
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d

  -- | Successor of a Cyclic enumeration.
  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

-- | Compass directions:
--
-- >>> csucc North
-- East
--
-- >>> cpred East
-- North
--
-- >>> minBound :: Direction
-- North
--
-- >>> maxBound :: Direction
-- South
data Direction = North | East | South | West
  deriving (Eq, Enum, Bounded, CyclicEnum, Show)

-- | Instructions for Compass turns.
data Turn = TNone | TLeft | TRight | TAround
  deriving (Eq, Enum, Bounded, Show)

-- | Get every direction in the compass.
every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

-- | Determine the next direction after a turn.
--
-- >>> rotate TLeft North
-- West
--
-- >>> rotate TRight West
-- North
--
-- >>> rotate TAround North
-- South
--
rotate :: Turn -> Direction -> Direction
rotate TNone   = id
rotate TLeft   = cpred
rotate TRight  = csucc
rotate TAround = cpred . cpred

-- | Determine the direction after a number of turns.
--
-- >>> rotateMany East [TLeft,TLeft,TLeft,TLeft] == East
-- True
--
rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)

-- | Get all directions when applying a list of turns.
--
-- >>> rotateManyTurns North [TNone,TAround]
-- [North,North,South]
--
-- >>> rotateManyTurns North every
-- [North,North,West,North,South]
--
rotateManyTurns :: Direction -> [Turn] -> [Direction]
rotateManyTurns = scanl (flip rotate)

-- | Find a turn instruction to change from an orientation to the given
-- direction. Any two directions can be reached from one another by one turn,
-- so we can safely use head here. There is always exactly one element in the
-- list.
orientate :: Direction -> Direction -> Turn
orientate d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

-- | Orientate many directions.
--
-- >>> orientateMany [North,East,South] [East,South,West]
-- [TRight,TRight,TRight]
--
orientateMany :: [Direction] -> [Direction] -> [Turn]
orientateMany = zipWith orientate
