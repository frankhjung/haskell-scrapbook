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

module RepMax (doRepMax, foldMax, main, repMax) where

-- | Fold version of 'repMax'.
-- Strictly this only works for positive integers. Whereas 'repMax' works
-- for all integers.
--
-- /"Everything's a fold"./
foldMax :: (Ord a, Num a) => [a] -> [a]
foldMax xs = xs'
  where
    m = if null xs then 0 else head xs
    (xs', largest) = foldl (\(b, c) a -> (largest : b, max a c)) ([], m) xs

-- | Repeat given maximum for entire list.
repMax :: [Int]           -- ^ list to replace
          -> Int          -- ^ maximum value used in replacement
          -> (Int, [Int]) -- ^ maximum and list with values replaced by maximum
repMax [] rep = (rep, [])
repMax [x] rep = (x, [rep])
repMax (x:xs) rep = (m', rep:xs')
  where (m, xs') = repMax xs rep
        m' = max m x

-- | Replace list with maximum value in the list.
doRepMax :: [Int]         -- ^ input list
            -> [Int]      -- ^ list with values replaced with maximum value
doRepMax xs = xs'
  where (largest, xs') = repMax xs largest

-- | Repeat maximum element from list.
main :: IO ()
main = let as = [-2,-3,-1,-4,-5]
  in print $ doRepMax as == foldMax as
