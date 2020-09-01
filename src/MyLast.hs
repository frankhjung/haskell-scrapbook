{-|

Module      : MyLast
Description : Explore last
Copyright   : © Frank Jung, 2020
License     : GPL-3

Play with last and penultimate of a list.

-}

module MyLast (myLast, myRev1, myRev2, penultimate) where

import           Control.Arrow ((>>>))

-- | Get second to last element of a list.
penultimate :: [a] -> Maybe a
penultimate []     = Nothing
penultimate [_]    = Nothing
penultimate [a,_]  = Just a
penultimate (_:as) = penultimate as

-- | Get last element of a list.
myLast :: [a] -> a
myLast = head . myRev1
-- myLast []     = error "No last element"
-- myLast [a]    = a
-- myLast (_:as) = myLast as

-- | Reverse a list, version 1.
myRev1 :: [a] -> [a]
myRev1 = foldl (flip (:)) []

-- | Reverse a list, version 2.
myRev2 :: [a] -> [a]
myRev2 as = foldr (\ a s -> (a :) >>> s) id as []
