#!/usr/bin/env runhaskell
{-|

Module      : CFold
Description : Continuation passing style, fold.
Copyright   : © Frank Jung, 2020
License     : GPL-3

Source: <https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Type_basics#Continuation_Passing_Style Yet Another Haskell Tutorial>

-}

module CFold (cfold', cfold, main) where

-- | CPS fold.
--
-- >>> λ> cfold' (\x t g -> (x : g t)) [] [1..10]
-- [1,2,3,4,5,6,7,8,9,10]
--
-- >>> λ> cfold' (\x t g -> g (x : t)) [] [1..10]
-- [10,9,8,7,6,5,4,3,2,1]
--
cfold' :: (t1 -> t2 -> (t2 -> t2) -> t2) -> t2 -> [t1] -> t2
cfold' _ z []     = z
cfold' f z (x:xs) = f x z (\y -> cfold' f y xs)

-- | Wrapper function to @cfold'@.
--
-- >>> λ> cfold (+) 0 [1..3]
-- 6
--
-- >>> λ> cfold (:) [] [1..3]
-- [1,2,3]
--
cfold :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
cfold f = cfold' (\ x t g -> f x (g t))

-- | Run examples.
main :: IO ()
main = do
  print $ cfold' (\x t g -> x : g t) [] [1..10]
  print $ cfold' (\x t g -> g (x : t)) [] [1..10]
  print $ cfold (+) 0 [1..3]
  print $ cfold (:) [] [1..3]

