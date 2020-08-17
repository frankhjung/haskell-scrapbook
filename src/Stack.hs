{-|

Module      : Stack
Description : Example using the State Monad
Copyright   : © Frank Jung, 2020
License     : GPL-3

Use the 'State' monad to implement a
<https://en.wikipedia.org/wiki/Stack_(abstract_data_type) Stack>.

Source:
<https://gist.github.com/sdiehl/8d991a718f7a9c80f54b sdiehl/state.hs>

-}

module Stack (
                Stack       -- 'Stack' as array of ints
              , empty       -- Empty 'Stack'
              , pop         -- Pop from top of 'Stack'
              , push        -- Push onto 'Stack'
              , top         -- Get element at top of 'Stack'
              , tasks       -- Run arbitrary tasks on 'Stack'
             ) where

import           State (State (..))

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
