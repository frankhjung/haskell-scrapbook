#!/usr/bin/env runhaskell

{-|

Module      : RepMax
Description : Repeat maximum element for entire list
Copyright   : © Frank Jung, 2020
License     : GPL-3

From
<https://danilafe.com/blog/haskell_lazy_evaluation/ Time Traveling In Haskell: How It Works And How To Use It> by Daniel Lafe.

Code for what Csongor calls the repMax problem:

* /Imagine you had a list, and you wanted to replace all the elements of/
  /the list with the largest element, by only passing the list once./

How can we possibly do this in one pass? First, we need to find the maximum
element, and only then can we have something to replace the other numbers
with! It turns out, though, that we can just expect to have the future
value, and all will be well.

In the function 'repMax' takes the list of integers, each of which it must
replace with their maximum element. It also takes as an argument the
maximum element, as if it had already been computed. It does, however,
still compute the intermediate maximum element, in the form of @m'@.
Otherwise, where would the future value even come from?

Thus far, nothing too magical has happened. It’s a little strange to expect
the result of the computation to be given to us; it just looks like wishful
thinking. The real magic happens in Csongor’s 'doRepMax' function.

Look, in particular, on the line with the @where@ clause. We see that
'repMax' returns the maximum element of the list, largest, and the
resulting list @xs'@ consisting only of /largest/ repeated as many times as
@xs@ had elements. But what’s curious is the call to 'repMax' itself. It
takes as input @xs@, the list we’re supposed to process and largest, the
value that /it itself returns/.

See also <https://kcsongor.github.io/time-travel-in-haskell-for-dummies/ Time travel in Haskell for dummies>
by Csongor Kiss.

== Example

Run 'doRepMax' or 'foldMax' over some lists:

>>> doRepMax [2,3,1,4,5]
[5,5,5,5,5]

>>> doRepMax [-2,-3,-1,-4,-5]
[-1,-1,-1,-1,-1]

-}

module RepMax (doRepMax, foldMax, main, repMax,traverseMax,traverseMax')  where

import           Data.Foldable    (toList)
import           Data.Maybe       (fromJust)
import           Data.Monoid      (First (..), getFirst)
import           Data.Traversable (mapAccumR)

-- | Fold version of 'repMax'.
-- Modified to also work with negative values.
-- /"Everything's a fold"./
foldMax :: (Integral a)
            => [a]        -- ^ list to change
            -> [a]        -- ^ list replaced with maximum element
foldMax xs = xs'
  where
    m = if null xs then 0 else head xs              -- initial max value
    (xs', largest) = foldl (\(b, c) a -> (largest : b, max a c)) ([], m) xs

-- | Generalise 'repMax' where input is a Traversable.
-- Seed value from @First@ of @Foldable@.
traverseMax :: (Traversable t, Integral a)
                => t a    -- ^ traversable input
                -> t a    -- ^ modified with maximum element
traverseMax t = xs'
  where
    m = fromJust $ getFirst $ foldMap (First . Just) t
    (largest, xs') = mapAccumR (\a b -> (max a b, largest)) m t

-- | Generalise 'repMax' where input is a Traversable.
-- Seed maximum value from head of list or 0 if empty.
traverseMax' :: (Traversable t, Integral a)
                => t a    -- ^ traversable input
                -> t a    -- ^ modified with maximum element
traverseMax' t = xs'
  where
    m = if null xs' then 0 else (head . toList) t   -- initial max value
    (largest, xs') = mapAccumR (\a b -> (max a b, largest)) m t

-- | Repeat given maximum for entire list.
repMax :: (Integral a)
            => [a]        -- ^ list to replace
            -> a          -- ^ maximum value used in replacement
            -> (a, [a])   -- ^ maximum and modified list maximum element
repMax [] rep = (rep, [])
repMax [x] rep = (x, [rep])
repMax (x:xs) rep = (m', rep:xs')
  where
    (m, xs') = repMax xs rep
    m' = max m x

-- | Replace list with maximum value in the list.
doRepMax :: (Integral a)
              => [a]      -- ^ input list
              -> [a]      -- ^ list with elements replaced with maximum value
doRepMax xs = xs'
  where
    (largest, xs') = repMax xs largest

-- | Repeat maximum element from list.
main :: IO ()
main =
  let xs = [-2,-3,-1,-4,-5] :: [Int]
  in print $ doRepMax xs == foldMax xs
              && foldMax xs == traverseMax xs
              && traverseMax xs == traverseMax' xs
