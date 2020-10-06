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

-- | Basic implementation.
sumTillNegative :: [Int] -> Int
sumTillNegative = sum . takeWhile (>= 0)

-- | Using fold with early termination.
sumTillNegative' :: [Int] -> Int
sumTillNegative' = go 0
  where
    go !total rest =
      case rest of
        [] -> total
        x:xs
          | x < 0     -> total
          | otherwise -> go (total + x) xs

-- Returns either the total (left value)
-- or the current acculation and the rest of the list.
-- The left value will terminate the loop.
-- See also
-- <https://hackage.haskell.org/package/base/docs/Prelude.html#v:either either>
foldTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
foldTerminate f accum0 list0 = either id id (go accum0 list0)
  where
    go !accum rest = do
      (x, xs) <- case rest of
                    []   -> Left accum      -- termination
                    x:xs -> Right (x, xs)   -- keep going
      accum' <- f accum x
      go accum' xs

-- | Returns either the total (left value)
-- or the current acculation and the rest of the list.
-- The left value will terminate the loop.
-- See also
-- <https://hackage.haskell.org/package/base/docs/Prelude.html#v:either either>
sumTillNegative'' :: [Int] -> Int
sumTillNegative'' = foldTerminate go 0
  where
    go !total x
      | x < 0     = Left total
      | otherwise = Right (total + x)
