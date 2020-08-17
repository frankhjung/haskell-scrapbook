{-|

Module      : State
Description : Deriving the State Monad
Copyright   : © Frank Jung, 2020
License     : GPL-3

A simple State monad implementation to explore it's characteristics.

Based on many articles, but principally from:
<http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/ The State Monad: A Tutorial for the Confused?>

-}

{-# LANGUAGE TupleSections #-}

module State (
                State(..)   -- State type with runState function
              , get         -- Get current state
              , put         -- Set current state
              , modify      -- Modify state function
              , evalState   -- Results of state function
              , execState   -- Final state
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
