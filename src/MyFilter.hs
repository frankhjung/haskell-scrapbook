{-|

Module      : MyFilter
Description : Implement filter using a fold
Copyright   : © Frank Jung, 2020
License     : GPL-3

Filter a list using fold.

-}

module MyFilter (myFilter) where

-- | Filter a list using foldr.
myFilter :: (Foldable t, Ord a) => (a -> Bool) -> t a -> [a]
myFilter p = foldr (\x z -> if p x then x : z else z) []
