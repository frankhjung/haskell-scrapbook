{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-

Use 'fix' to recurse rather than explicit recursion.

Modified from algorithm described in
<https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html Introduction to recursion schemes>.

Next, build a fixed-point combinator in the type system.

@
type Y t = t (t (t (t (t ...))))
@

That is a data type @Y@ that, when given another type @f@, wraps an @f@
whose children are of type @(Y f)@.

-}

module Main (main) where

import           Data.Function      (fix)
import           Numeric.Natural    (Natural)
import           System.Environment (getArgs)
import           Text.Read          (readMaybe)

-- | Usage message
usage :: String -> IO ()
usage = putStr . unlines . (:[ "Usage: [number]"
                           , "Count up to a given natural number."
                           , "Count down from a given natural number."
                           ])

-- | Run the counter.
runCounter :: Natural -> IO ()
runCounter n = mapM_ print [countUp n, countDown n]

-- | A counter from 0 to a natural number, `n`.
-- Returns the same as @[0..n]@
--
-- In this implementation, we use fix to define a recursive function loop
-- that takes a list of natural numbers and returns a list of natural
-- numbers. The loop function checks if the first element of the list is
-- greater than or equal to 1, and if so, it returns the result of calling
-- loop with a new list that starts with x - 1 and is followed by the
-- original list. If the first element of the list is less than 1, the loop
-- function returns the original list. We then use fix to define countUp as
-- the fixed point of loop, where the initial list is [n].
--
-- Traditional recursive implementation of `countUp`.
-- @
-- countUp' :: Natural -> [Natural]
-- countUp' n = loop n [n]
--   where
--     loop x xs
--       | x >= 1 = loop (x - 1) (x - 1 : xs)
--       | otherwise = xs
-- @
countUp :: Natural -> [Natural]
countUp c = fix (\loop ns@(n:_) -> if n >= 1 then loop (n - 1:ns) else ns) [c]

-- | Count down from a natural number, `n` to 0.
-- Returns the same as @[n,n-1..0]@
--
-- The fix function is used to define a recursive function loop that takes
-- a natural number n and returns a list of natural numbers. The loop
-- function is defined using a lambda expression that takes two arguments:
-- loop and n. The loop function checks if n is greater than 0, and if so,
-- it returns a list that starts with n and is followed by the result of
-- calling loop with n - 1. If n is not greater than 0, the loop function
-- returns a list that contains only 0.
--
-- Traditional recursive implementation of `countDown`.
-- @
-- countDown' :: Natural -> [Natural]
-- countDown' = loop
--   where
--     loop x
--       | x > 0 = x : loop (x - 1)
--       | otherwise = [0]
-- @
countDown :: Natural -> [Natural]
countDown = fix (\loop n -> if n > 0 then n : loop (n - 1) else [0])

-- | Read counter from a command line argument.
-- Count up to a given natural number.
-- Count down from a given natural number.
main :: IO ()
main = maybe (usage "Error: expected one numeric argument")
          runCounter . readMaybe . head =<< getArgs
