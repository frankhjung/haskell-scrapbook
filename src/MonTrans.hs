{-|

Module      : MonTrans
Description : Explorer Monad Transformers
Copyright   : © Frank Jung, 2020
License     : GPL-3

Based off
<https://www.fpcomplete.com/haskell/tutorial/monad-transformers/ FPComplete Monad Transformers>

-}

{-# LANGUAGE BangPatterns #-}

module MonTrans ( sumTillNegative
                , sumTillNegative'
                ) where

sumTillNegative :: [Int] -> Int
sumTillNegative = sum . takeWhile (>= 0)

sumTillNegative' :: [Int] -> Int
sumTillNegative' =
    go 0
  where
    go !total rest =
      case rest of
        [] -> total
        x:xs
          | x < 0     -> total
          | otherwise -> go (total + x) xs
