#!/usr/bin/env runhaskell

{-|

Module      : State
Description : Deriving the State Monad
Copyright   : © Frank Jung, 2020
License     : GPL-3

From
<https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html Deriving the State Monad by William Yaoh>

> echo 1 2 3 4 5 6 | runhaskell state.hs
> (2,["1","2","3","4","5","6"])

-}

{-# LANGUAGE TupleSections #-}

module State (State(..), reverseWithCount, reverseTwiceWithCount ) where

-- | State type is defined with a stateful function that takes takes in
-- a current state and returns an updated state along with its "normal"
-- return value.
--
-- Here 'runState' is of type:
--
-- > runState :: State s a -> s -> (s, a)
--
newtype State s a = State { runState :: s -> (s, a) }

-- state :: (s -> (a, s)) -> State s a

-- TODO define a State class with get, put and modify operations.

-- Returns current state.
get :: State s s
get = State (\s -> (s,s))

-- Replace current state with the given value. Returns unit.
put :: s -> State s ()
put s = State (const (s, ()))

-- Update current state using the given function
modify :: (s -> s) -> State s ()
modify f = get >>= put . f

-- TODO define show instance
-- Show count.
-- showCount :: State Int [String] -> String
-- showCount s = do
--   (s', a') <- get
--   show (s', a')

-- Functor
instance Functor (State s) where
  fmap f (State stateFx) =
    State (\s -> let (s', fx) = stateFx s
                 in (s', f fx))

-- Applicative
instance Applicative (State s) where
  pure x = State (,x)
  (<*>) (State stateFx) (State nextFx) =
    State (\s -> let (s', fx) = stateFx s
                     (s'', ffx) = nextFx s'
                 in (s'', fx ffx))

-- Monad
instance Monad (State s) where
  return = pure
  (>>=) (State stateFx) nextFx =
    State (\s -> let (s', fx) = stateFx s
                 in runState (nextFx fx) s')

-- | Reverse a list, and increase a count of function calls.
reverseWithCount :: [String] -> State Int [String]
reverseWithCount list = State (\s -> (s + 1, reverse list))

-- | Reverse a list twice giving back original but incremented call count.
reverseTwiceWithCount :: [String] -> State Int [String]
reverseTwiceWithCount list = reverseWithCount list
                             >>= reverseWithCount
                             >>= (\ls -> State (\s -> (s+1, ls)))

-- Reverse list of contents from command line.
-- main :: IO ()
-- main = interact $ show . reverseTwiceWithCount . words

