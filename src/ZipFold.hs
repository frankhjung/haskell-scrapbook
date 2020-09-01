{-|

Module      : ZipFold
Description : Zipping with folds
Copyright   : © Frank Jung, 2020
License     : GPL-3

From
<https://doisinkidney.com/posts/2020-08-22-some-more-list-algorithms.html Zipping with Folds>
which is based on
<http://okmij.org/ftp/Streams.html#zip-folds How to Zip with Folds>.

The 'Prelude's version of 'zip' uses recursion:

@
zip :: [a] -> [b] -> [(a,b)]
zip []     _bs    = []
zip _as    []     = []
zip (a:as) (b:bs) = (a,b) : zip as bs
@

But, this won't work for streams. The version included below /does/ work with
streams. It uses a recursive type to represent the stream data. This
matches from the beginning of the stream. You could match from the end of
the stream by swapping 'foldr' with 'foldl'.

-}

module ZipFold (Zip(..), zip) where

import           Prelude hiding (zip)

-- | Defines a recursive type.
newtype Zip a b = Zip { runZip :: a -> (Zip a b -> b) -> b }

-- | Function to fuse two folds.
zip :: [a] -> [b] -> [(a,b)]
zip xs ys = foldr xf xb xs (Zip (foldr yf yb ys))
  where
    xf x xk yk = runZip yk x xk
    xb _ = []

    yf y yk x xk = (x,y) : xk (Zip yk)
    yb _ _ = []
