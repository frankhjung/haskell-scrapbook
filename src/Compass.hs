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
                , orient
                ) where

-- | ToDo
--
-- * add @rotateMany@
-- * add @rotateManySteps@
-- * add @orientMany@

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
rotate :: Turn -> Direction -> Direction
rotate TNone   = id
rotate TLeft   = cpred
rotate TRight  = csucc
rotate TAround = cpred . cpred

-- | Find a turn instruction to change from an orientation to the given
-- direction. Any two directions can be reached from one another by one turn,
-- so we can safely use head here. There is always exactly one element in the
-- list.
orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every
