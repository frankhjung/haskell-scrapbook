{-|

Module      : MyFilter
Description : Implement filter using a fold
Copyright   : © Frank Jung, 2020
License     : GPL-3

Filter a list using fold. In the Prelude,
<https://hackage.haskell.org/package/base/docs/Prelude.html#v:filter filter>
is defined as a recursive function:

@
  filter :: (a -> Bool) -> [a] -> [a]
  filter _pred []    = []
  filter pred (x:xs)
    | pred x         = x : filter pred xs
    | otherwise      = filter pred xs
@

-}

module MyFilter (myFilter) where

-- | Filter a list using foldr.
myFilter :: (Foldable t, Ord a) => (a -> Bool) -> t a -> [a]
myFilter p = foldr (\x z -> if p x then x : z else z) []
