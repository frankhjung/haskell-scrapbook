#!/usr/bin/env runhaskell

{-|

Module      : State
Description : Deriving the State Monad
Copyright   : © Frank Jung, 2020
License     : GPL-3

From
<https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html Deriving the State Monad by William Yaoh>

>>> echo 1 2 3 4 5 6 | runhaskell state.hs
(2,["1","2","3","4","5","6"])

-}

module State (main) where

data State s a = State { runState :: s -> (s, a) }

-- define show
instance (Show s, Show a) => Show (State s a) where
  show (State s a) = "(" ++ show s ++ ", " ++ show a ++ ")"

-- define Functor
-- define Applicative
-- define Monad

-- | Reverse a list, and increase a count of function calls.
reverseWithCount :: [a] -> State Int [a]
reverseWithCount list = State (\s -> (s + 1, reverse list))

-- | Reverse a list twice giving back original but incremented call count.
reverseTwiceWithCount :: [a] -> State Int [a]
reverseTwiceWithCount list = reverseWithCount list
                             >>= reverseWithCount
                             >>= (\list1 -> State (\s -> (s +1, list1)))

-- Reverse list of contents from command line.
main :: IO ()
main = interact $ show . reverseTwiceWithCount . words

