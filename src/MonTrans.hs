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
                , sumTillNegative''
                ) where

sumTillNegative :: [Int] -> Int
sumTillNegative = sum . takeWhile (>= 0)

sumTillNegative' :: [Int] -> Int
sumTillNegative' = go 0
  where
    go !total rest =
      case rest of
        [] -> total
        x:xs
          | x < 0     -> total
          | otherwise -> go (total + x) xs

foldTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
foldTerminate f = go
  where
    go !accum rest =
      case rest of
        [] -> accum
        x:xs ->
          case f accum x of
            Left accum'  -> accum'          -- early termination
            Right accum' -> go accum' xs    -- keep going

sumTillNegative'' :: [Int] -> Int
sumTillNegative'' = foldTerminate go 0
  where
    go !total x
      | x < 0     = Left total
      | otherwise = Right (total + x)
