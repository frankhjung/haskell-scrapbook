{-|

Module      : Stack
Description : Example using the MyState Monad
Copyright   : © Frank Jung, 2020
License     : GPL-3

Use the 'MyState' monad to implement a
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

import           MyState (MyState (..))

-- | Stack as array of intergers.
type Stack = [Int]

-- | Empty stack.
empty :: Stack
empty = []

-- | Remove element from top of stack.
pop :: MyState Stack Int
pop = MyState $ \(x:xs) -> (x, xs)

-- | Push element onto stack.
push :: Int -> MyState Stack ()
push a = MyState $ \xs -> ((), a:xs)

-- | Return element at top of stack.
top :: MyState Stack Int
top = MyState $ \(x:xs) -> (x, x:xs)

-- | Example usage of 'MyState' 'Stack'. 'push' & 'pop' some elements onto
-- the stack, and read the current 'top' element.
tasks :: MyState Stack Int
tasks = do
  push 1        -- populate with some data
  push 2
  push 3
  a <- pop      -- 3
  b <- pop      -- 2
  push (a + b)  -- 5
  top           -- 5
