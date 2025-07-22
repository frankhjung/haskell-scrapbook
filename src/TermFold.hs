{-|

Module      : TermFold
Description : Explorer Early Termination with Folds
Copyright   : © Frank Jung, 2020
License     : GPL-3

This module explores different techniques for folding over a list with
the ability to terminate early. It serves as a practical example of
control flow patterns in Haskell, including strict folds,
continuation-passing style (CPS), and monadic control flow with `Either`.

This module focuses on the foundational concept of using a monad (`Either`)
for short-circuiting.

Code based on
<https://www.fpcomplete.com/haskell/tutorial/monad-transformers/ FPComplete Folds with early termination>

The functions are defined to sum a list of integers, stopping
when a negative value is encountered.

  (1) `sumTillNegative`: A baseline implementation using `sum` and `takeWhile`.
  2. `sumTillNegative'`: A strict, tail-recursive version.
  3. `sumTillNegative''`: An implementation using `foldr` with continuation-passing style.
  4. `sumTillNegative'''`: Uses a generic `_foldTerminate` helper that employs the `Either` monad for early exit.
-}

{-# LANGUAGE BangPatterns #-}

module TermFold ( sumTillNegative     -- | baseline implementation using `sum` and `takeWhile`
                , sumTillNegative'    -- | strict, tail-recursive version
                , sumTillNegative''   -- | continuation-passing style with `foldr`
                , sumTillNegative'''  -- | uses `Either` for early exit
                ) where

-- | Basic implementation.
sumTillNegative :: [Int] -> Int
sumTillNegative = sum . takeWhile (>= 0)

-- | Fold strict sum with early termination.
--
-- This function:
--  * uses a strict accumulator (`total`) for efficiency
--  * recursively adds values until a negative is found or the list ends
--  * returns the sum up to (but not including) the first negative
sumTillNegative' :: [Int] -> Int
sumTillNegative' = go 0
  where
    go !total rest =
      case rest of
        [] -> total
        x:xs
          | x < 0     -> total
          | otherwise -> go (total + x) xs

-- | Using foldr with continuation passing style.
-- This approach leverages Haskell's laziness by using continuations to
-- short-circuit computation when encountering a negative value.
--
-- This function:
--  * Single-pass processing
--  * Clean, declarative style
--  * Explicit control over strictness
sumTillNegative'' :: [Int] -> Int
sumTillNegative'' xs = foldr step id xs 0
  where
    step x cont acc
      | x < 0     = acc  -- Early termination
      | otherwise = cont (acc + x)

-- | Returns either the total (left value)
-- or the current accumulation and the rest of the list.
-- The left value will terminate the loop.
-- See also
-- <https://hackage.haskell.org/package/base/docs/Prelude.html#v:either either>
sumTillNegative''' :: [Int] -> Int
sumTillNegative''' = _foldTerminate go 0
  where
    go !total x
      | x < 0     = Left total
      | otherwise = Right (total + x)

-- Returns either the total (left value) or the current accumulation and the
-- rest of the list.
--
-- This function:
--  * generalizes fold that can terminate early
--  * the folding function returns Left to terminate, or Right to continue.
--  * returns the accumulated value when terminated.
-- See also
-- <https://hackage.haskell.org/package/base/docs/Prelude.html#v:either either>
_foldTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
_foldTerminate f accum0 list0 = either id id (go accum0 list0)
  where
    go !accum rest = do
      (x, xs) <- case rest of
                    []   -> Left accum      -- termination
                    x:xs -> Right (x, xs)   -- keep going
      accum' <- f accum x
      go accum' xs
