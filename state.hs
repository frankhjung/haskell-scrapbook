#!/usr/bin/env runhaskell

{-|

Module      : State
Description : Deriving the State Monad
Copyright   : © Frank Jung, 2020
License     : GPL-3

A simple State Monad implementation to explore it's characteristics.
Based on many articles, but principally from:

- <http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/ The State Monad: A Tutorial for the Confused?>
- <https://gist.github.com/sdiehl/8d991a718f7a9c80f54b sdiehl / state.hs>

> runhaskell state.hs
>

-}

{-# LANGUAGE TupleSections #-}

module State (
             evalState,
             execState,
             get,
             main,
             modify,
             pop,
             push,
             put,
             State(..),
             tasks,
             top,
             ) where

-- | Mock of State data type. (Not yet a Monad, Functor or Applicative.)
newtype State s a = State { runState :: s -> (a, s) }

-- | Returns current state.
get :: State s s
get = State $ \s -> (s, s)

-- | Replace current state with the given value. Returns unit.
put :: s -> State s ()
put s = State $ const ((), s)

-- | Update current state using the given function.
modify :: (s -> s) -> State s ()
modify f = get >>= put . f

-- | Returns results of state function.
evalState :: State s a -> s -> a
evalState f = fst . runState f

-- | Returns final state.
execState :: State s a -> s -> s
execState f = snd . runState f

-- Functor
instance Functor (State s) where
  fmap f (State stateFx) =
    State (\s -> let (fx, s') = stateFx s
                 in (f fx, s'))

-- Applicative
instance Applicative (State s) where
  pure x = State (x,)
  (<*>) (State stateFx) (State nextFx) =
    State (\s -> let (fx, s')   = stateFx s
                     (ffx, s'') = nextFx s'
                 in (fx ffx, s''))

-- Monad
instance Monad (State s) where
  return a = State (a,)
  -- Bind signature:
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  -- (>>=) :: (s -> (a, s)) -> (a -> s -> (b, s)) -> (s -> (b, s))
  (>>=) (State stateFx) nextFx =
    State (\s -> let (fx, s') = stateFx s
                 in runState (nextFx fx) s')

-- Example

-- | Stack as array of intergers.
type Stack = [Int]

-- | Empty stack.
empty :: Stack
empty = []

-- | Remove element from top of stack.
pop :: State Stack Int
pop = State $ \(x:xs) -> (x, xs)

-- | Push element onto stack.
push :: Int -> State Stack ()
push a = State $ \xs -> ((), a:xs)

-- | Return element at top of stack.
top :: State Stack Int
top = State $ \(x:xs) -> (x, x:xs)

-- | Example usage of 'State' 'Stack'. 'push' & 'pop' some elements onto
-- the stack, and read the current 'top' element.
tasks :: State Stack Int
tasks = do
  push 1        -- populate with some data
  push 2
  push 3
  a <- pop      -- 2
  b <- pop      -- 3
  push (a + b)  -- 5
  top           -- 5

-- | Run example showing top of 'Stack' and final 'State'.
--
-- > runhaskell state.hs
-- > 5
-- > [5,1]
main :: IO ()
main = do
  print $ evalState tasks empty   -- 5
  print $ execState tasks empty   -- [5,1]

