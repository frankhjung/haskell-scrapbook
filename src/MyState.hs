{-|

Module      : MyState
Description : Deriving the State Monad
Copyright   : © Frank Jung, 2020,2021,2022
License     : GPL-3

A simple State monad implementation to explore it's characteristics.

Based on many articles, but principally from:
<http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/ The State Monad: A Tutorial for the Confused?>

-}

{-# LANGUAGE TupleSections #-}

module MyState (
                MyState(..)   -- MyState type with runState function
              , get         -- Get current state
              , put         -- Set current state
              , modify      -- Modify state function
              , evalState   -- Results of state function
              , execState   -- Final state
             ) where

-- | Mock of MyState data type. (Not yet a Monad, Functor or Applicative.)
newtype MyState s a = MyState { runState :: s -> (a, s) }

-- | Returns current state.
get :: MyState s s
get = MyState $ \s -> (s, s)

-- | Replace current state with the given value. Returns unit.
put :: s -> MyState s ()
put s = MyState $ const ((), s)

-- | Update current state using the given function.
modify :: (s -> s) -> MyState s ()
modify f = get >>= put . f

-- | Returns results of state function.
evalState :: MyState s a -> s -> a
evalState f = fst . runState f

-- | Returns final state.
execState :: MyState s a -> s -> s
execState f = snd . runState f

-- Functor
instance Functor (MyState s) where
  fmap f (MyState stateFx) =
    MyState (\s -> let (fx, s') = stateFx s
                 in (f fx, s'))

-- Applicative
instance Applicative (MyState s) where
  pure x = MyState (x,)
  (<*>) (MyState stateFx) (MyState nextFx) =
    MyState (\s -> let (fx', s')   = stateFx s
                       (fx'', s'') = nextFx s'
                   in (fx' fx'', s''))

-- Monad
instance Monad (MyState s) where
  return a = MyState (a,)
  -- Bind signature:
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  -- (>>=) :: MyState s a -> (a -> MyState s b) -> MyState s b
  -- (>>=) :: (s -> (a, s)) -> (a -> s -> (b, s)) -> (s -> (b, s))
  (>>=) (MyState stateFx) nextFx =
    MyState (\s -> let (fx, s') = stateFx s
                 in runState (nextFx fx) s')
