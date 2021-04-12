#!/usr/bin/env runhaskell

{- HLINT ignore "Use module export list" -}

{-|

= FPComplete: covariance and contravariance

Examples from
<https://www.fpcomplete.com/blog/2016/11/covariance-contravariance/ Covariance and Contravariance by Michael Snoyman>.

== Summary

  (1) in __covariance__, both the original and lifted functions point in the same direction (from a to b)
  2. in __contravariance__, the original and lifted functions point in opposite directions (one goes from a to b, the other from b to a)

@
fmap :: Functor f => (a -> b) -> f a -> f b
mapMakeString :: (b -> a) -> t a -> t b
@

Here, the 'Functor' typeclass in Haskell as a covariant functor.

To run this call either of these:

@
stack exec fpcomplete
runghc -Wno-unrecognised-pragmas app/FPComplete.hs
@

-}

module Main (main) where

import           Data.Functor.Contravariant (Predicate (..), contramap,
                                             getPredicate)

showInt' :: Int -> String
showInt' = show

maybeInt :: Maybe Int
maybeInt = Just 5

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe = fmap

maybeString :: Maybe String
maybeString = fmapMaybe showInt' maybeInt

newtype MakeString a = MakeString { makeString :: a -> String }

-- | showInt
--
-- λ> makeString showInt 4
-- "4"
showInt :: MakeString Int
showInt = MakeString show

-- | plus3ShowInt - bad as this does not compose.
--
-- λ> makeString plus3ShowInt 4
-- "7"
plus3ShowInt :: MakeString Int
plus3ShowInt = MakeString (show . (+ 3))

-- | mapMakeString - can we map this?
--
-- This is not the same as fmap though as:
--
--  fmap :: Functor f => (a -> b) -> f a -> f b
--
-- Where as:
--
--  mapMakeString :: (b -> a) -> t a -> t b
--
-- Here t :: MakeString
--
-- Explanation:
--
-- MakeString (g . f) => MakeString ( a -> b $  b -> a) => MakeString b
--
mapMakeString :: (b -> a) -> MakeString a -> MakeString b
mapMakeString f (MakeString g) = MakeString (g . f)

-- | plus4ShowInt4
--
-- λ> makeString plus4ShowInt 4
-- "8"
plus4ShowInt :: MakeString Int
plus4ShowInt = mapMakeString (+ 4) showInt


{-
 -
== Example: filtering with Predicate

See
<https://hackage.haskell.org/package/base/docs/Data-Functor-Contravariant.html#t:Predicate Predicate>.

  (1) first example using function composition
  2.  use functions and Predicate type to wrap up helper functions and avoid explicit function composition

-}

-- | Filter ints greater than 3.
greaterThanThree' :: Int -> Bool
greaterThanThree' = (> 3)

-- | Filter length greater than 3.
lengthGTThree' :: [a] -> Bool
lengthGTThree' = greaterThanThree' . length

-- | Example filter English words for number 1..10 greater than 3.
englishGTThree' :: Int -> Bool
englishGTThree' = lengthGTThree' . english


-- | Filter ints greater than 3.
greaterThanThree :: Predicate Int
greaterThanThree = Predicate (> 3)

-- | Filter length greater than 3.
--
-- @
-- contramap :: (b -> a) -> f a -> f b
-- contramap (g . f) = contramap f . contramap g
-- @
lengthGTThree :: Predicate [a]
lengthGTThree = contramap length greaterThanThree

-- | Example filter English words for number 1..10 greater than 3.
--
-- 'contramap' :: (a' -> a) -> f a -> f a'
englishGTThree :: Predicate Int
englishGTThree = contramap english lengthGTThree

-- | English words for numbers 1 on to 10.
english :: Int -> String
english 1  = "one"
english 2  = "two"
english 3  = "three"
english 4  = "four"
english 5  = "five"
english 6  = "six"
english 7  = "seven"
english 8  = "eight"
english 9  = "nine"
english 10 = "ten"
english _  = "unknown"

-- Run examples.
main :: IO ()
main = do
  print maybeString
  putStrLn $ "plus3ShowInt 4 = " ++ makeString plus3ShowInt 4
  putStrLn $ "plus4ShowInt 4 = " ++ makeString plus4ShowInt 4
  putStrLn "Names of numbers greater than 3 using function composition:"
  print $ filter englishGTThree' [1..10]
  putStrLn "Names of numbers greater than 3 using contravariant functions:"
  print $ filter (getPredicate englishGTThree) [1..10]
