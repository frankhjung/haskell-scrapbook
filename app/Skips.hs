#!/usr/bin/env runhaskell

{-|

Module      : Skips
Description : Implementation skips homework problem.
Copyright   : © Frank Jung, 2020
License     : GPL-3

I was looking for more code examples of 'mapAccumL' and 'mapAccumR'
functions and came across this blog
<https://medium.com/@robinriclet/climbing-the-haskell-curve-my-first-mapaccuml-2b86420a600d Climbing the Haskell curve : my first mapAccumL>.
This is my implementation of the problem described in that blog.

== Example

Run with as script:

@
$ runhaskell app/Skips.hs abcd
Loaded package environment from /home/frank/.ghc/x86_64-linux-8.8.3/environments/default
["abcd","bd","c","d"]
@

Or, run as compiled program:

@
$ stack exec
skips abcd > ["abcd","bd","c","d"]
@

== Notes

To generate Haddock documentation:

@
haddock --odir public --html --hyperlinked-source app/Skips.hs
@

-}

module Main(main) where

import           Data.Traversable   (mapAccumL)
import           System.Environment (getArgs)

-- | From an input list, generate a list of /skipped/ items.
--
-- The output of 'skips' is a list of lists. The first list in the output
-- is the same as the input list. The second list in the output contains
-- every second element from the input list. In general, the /n/th list in
-- the output contains every /n/th element from the input list.
--
skips :: [a]            -- ^ input list
          -> [[a]]      -- ^ list of skipped items
skips xs = snd $ mapAccumL accum 1 xs'
  where
    xs' = replicate (length xs) xs
    accum :: Int -> [a] -> (Int, [a])
    accum c x = (c + 1, itemn c x)
    itemn :: Int -> [a] -> [a]
    itemn c x = map snd (filter (\nx -> mod (fst nx) c == 0) (zip [1..] x))

-- | Run skips function reading an input string from the command line.
main :: IO ()
main = getArgs >>= mapM_ (print . skips)
