{-# LANGUAGE RankNTypes #-}
{-|

Module      : ApplyToTuple
Description : Example rank-n-type
Copyright   : © Frank Jung, 2021
License     : GPL-3

Example rank-n-type from
<http://sleepomeno.github.io/blog/2014/02/12/Explaining-Haskell-RankNTypes-for-all Explaining Haskell RankNTypes for all>.
-}

module ApplyToTuple (applyToTuple) where

-- | Example of a rank N type. This will apply the function to the provided
-- input tuple.
--
-- >>> applyToTuple length ("foo", [1,2,3,4])
-- (3,4)
applyToTuple :: forall b c. (forall a. [a] -> Int) -> ([b], [c]) -> (Int, Int)
applyToTuple f (xs, ys) = (f xs, f ys)
