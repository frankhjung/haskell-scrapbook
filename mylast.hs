#!/usr/bin/env runhaskell

{-|

Module      : MyLast
Description : Explore last.
Copyright   : © Frank Jung, 2020
License     : GPL-3

Play with last and penultimate of a list.

-}

module MyLast (main) where

import           Control.Arrow ((>>>))
import           Data.Maybe    (Maybe (..), isNothing)


-- | Get last element of a list.
myLast :: [a] -> a
myLast = head . myRev1
-- myLast []     = error "No last element"
-- myLast [a]    = a
-- myLast (_:as) = myLast as

test1 :: IO ()
test1 =
  let as = [1..5]
  in print $ last as == myLast as

test2 :: IO ()
test2 =
  let as = [4]
  in print $ last as == myLast as

-- | Get second to last element of a list.
penultimate :: [a] -> Maybe a
penultimate []       = Nothing
penultimate [_]      = Nothing
penultimate [a,_]    = Just a
penultimate (_:a:as) = penultimate (a:as)

test3 :: IO ()
test3 =
    let as = [1]
    in print $ isNothing (penultimate as)

test4 :: IO ()
test4 =
    let as = [1,2]
    in print $ Just 1 == penultimate as

test5 :: IO ()
test5 =
    let as = [1..5]
    in print $ Just (((!! 1) . reverse) as) == penultimate as

-- | Reverse a list, version 1.
myRev1 :: [a] -> [a]
myRev1 = foldl (flip (:)) []

-- | Reverse a list, version 2.
myRev2 :: [a] -> [a]
myRev2 as = foldr (\ a s -> (a :) >>> s) id as []

test6 :: IO ()
test6 =
    let as = [1..5]
    in print $ last as == (head . myRev1) as

test7 :: IO ()
test7 =
    let as = [1..5]
    in print $ myRev1 as == myRev2 as

-- | Test myLast
main :: IO ()
main =
  test1
  >> test2
  >> test3
  >> test4
  >> test5
  >> test6
  >> test7

