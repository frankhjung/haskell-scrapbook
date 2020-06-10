#!/usr/bin/env runhaskell

{-|

Module      : MyLast
Description : Explore last.
Copyright   : © Frank Jung, 2020
License     : GPL-3

Play with last and penultimate of a list.

-}

module MyLast (main) where

import           Data.Maybe (Maybe (..), isNothing)

-- | Get last element of a list.
myLast :: [a] -> a
myLast []     = error "No last element"
myLast [a]    = a
myLast (_:as) = myLast as

test1 :: IO ()
test1 =
  let as = [1,2,3]
  in print $ last as == myLast as

test2 :: IO ()
test2 =
  let as = [4]
  in print $ last as == myLast as

penultimate :: [a] -> Maybe a
penultimate []       = Nothing
penultimate [_]      = Nothing
penultimate [a,_]    = Just a
penultimate (_:a:as) = penultimate (a:as)

test3 :: IO ()
test3 =
    let as = [1,2]
    in print $ Just 1 == penultimate as

test4 :: IO ()
test4 =
    let as = [1]
    in print $ isNothing (penultimate as)

test5 :: IO ()
test5 =
    let as = [1,2,3,4,5]
    in print $ Just 4 == penultimate as

-- | Test myLast
main :: IO ()
main =
  test1
  >> test2
  >> test3
  >> test4
  >> test5

