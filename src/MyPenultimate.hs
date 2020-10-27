{-|

Module      : MyPenultimate
Description : Get penultimate entry of a list
Copyright   : © Frank Jung, 2020
License     : GPL-3

Get the penultimate of a list.

-}

module MyPenultimate (penultimate) where

-- | Get second to last element of a list.
penultimate :: [a] -> Maybe a
penultimate []     = Nothing
penultimate [_]    = Nothing
penultimate [a,_]  = Just a
penultimate (_:as) = penultimate as
