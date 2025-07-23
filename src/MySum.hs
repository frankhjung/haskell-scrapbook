{-|

Module      : MySum
Description : A module to compute the sum of a list of numbers.
Copyright   : © Frank Jung, 2025
License     : GPL-3

This is a simple example of using the 'ST' Monad for stateful summation of a
list.

The 'ST' monad in Haskell allows you to perform mutable (stateful) computations
in a safe, local way. It stands for "State Thread" and is defined in
Control.Monad.ST ('ST').

== Key Points

[Local Mutability] You can create and modify mutable variables ('STRef's)
  inside the 'ST' monad.
[Safety] Changes made in the 'ST' monad are not visible outside; the
  mutation is encapsulated.
[Pure Interface] When you run an 'ST' computation (using 'runST'), you get a
  pure result, and the mutable state cannot escape.

== Example usage

Computes the sum of a list using the 'mySum' function:

@
let total = mySum [1, 2, 3, 4, 5]
@

-}

module MySum (
    mySum
) where

import           Control.Monad.ST
import           Data.STRef

-- | Computes the sum of a list of integers using the ST monad.
sumSt ::
    [Int]                     -- ^ The list of integers to sum
    -> STRef s Int            -- ^ A mutable reference to accumulate the sum
    -> ST s Int               -- ^ The stateful computation that returns the sum
sumSt []     _ = return 0           -- Base case: empty list returns 0
sumSt (x:xs) accumRef = do
    accum <- readSTRef accumRef     -- Read the current value of the accumulator
    writeSTRef accumRef (accum + x) -- Update the accumulator with the new value
    sumSt xs accumRef               -- Recursively for the remaining elements

-- | Computes the sum of a list of integers in a stateful manner.
mySum :: [Int] -> Int
mySum xs = runST $ do
    accumRef <- newSTRef 0    -- Create a new STRef to hold the accumulator
    _ <- sumSt xs accumRef    -- Compute the sum using the ST monad
    readSTRef accumRef        -- Read the final sum from the STRef
