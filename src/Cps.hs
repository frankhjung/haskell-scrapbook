{-# LANGUAGE RankNTypes #-}

{-|

Module      : Cps
Description : Continuation passing style, examples
Copyright   : © Frank Jung, 2020
License     : GPL-3

Example from [Wikibooks Continuation Passing
Style](https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style)

This uses the 'Cont' Monad from the
[transformers](https://hackage.haskell.org/package/transformers) package.

Using 'callCC' is better than using return as argument is a function and it
returns a suspended computation.

A function of type @a -> b@ would become @a -> (b -> r) -> r@ in CPS, where
@b -> r@ is the continuation.

-}

module Cps
  ( -- * Types
    CPS (..)
    -- * Custom continuation passing style (CPS)
  , toCPS
  , fromCPS
    -- * CPS Function Examples
  , addCont
  , addOne
  , pythagorasCont
  , releaseString
  , releaseStringCPS
  , squareCont
  , withOS
  , withTimestamp
  , withVersionNumber
  ) where

import           Control.Monad.Trans.Cont (Cont, callCC)

-- | Custom continuation passing style.
--
-- The following example uses a custom continuation-passing style (CPS) to
-- demonstrate the general concept.
--
-- This converts any value into a suspended computation.
--
-- @flip ($) :: b -> (b -> c) -> c@
--
-- Longhand this is:
-- @
-- toCPS a = \k -> k a
-- @
--
-- Or more simply as:
-- @
-- toCPS a k = k a
-- @
toCPS :: a -> (forall r. (a -> r) -> r)
toCPS = flip ($)

-- | From custom continuation.
--
-- The following example calls the continuation function with 'id'.
--
-- Passing 'id' to the continuation function will return the value.
--
-- @($ id) :: ((a -> a) -> b) -> b@
--
-- Longhand this is:
-- @
-- fromCPS f = f id
-- @
fromCPS :: (forall r. (a -> r) -> r) -> a
fromCPS = ($ id)

-- | Continuation passing style (CPS) examples.
--
-- The following uses the 'Cont' Monad from the 'transformers' package.

-- | Continuation for add function.
addCont :: Int -> Int -> Cont r Int
addCont x y = callCC $ \k -> k (x + y)

-- | Continuation for square function.
squareCont :: Int -> Cont r Int
squareCont x = callCC $ \k -> k (x * x)

-- | Continuation for pythagoras function.
pythagorasCont :: Int -> Int -> Cont r Int
pythagorasCont x y = do
    x_squared <- squareCont x
    y_squared <- squareCont y
    addCont x_squared y_squared

-- | Type CPS for Continuation-passing style.
--
-- See CpsSpec.hs for usage.
newtype CPS a = CPS { runCPS :: forall r. (a -> r) -> r}

-- | Functor instance for 'CPS'.
instance Functor CPS where
  fmap f (CPS g) = CPS $ \k -> g (k . f)

-- | Application instance for 'CPS'.
instance Applicative CPS where
  pure a = CPS ($ a)
  (CPS f) <*> (CPS g) = CPS $ \k -> f (\a -> g (k . a))

-- | Monad instance for 'CPS'.
instance Monad CPS where
  return a = CPS ($ a)
  (CPS g) >>= f = CPS $ \k -> g (\a -> runCPS (f a) k)

-- Define a simple function to apply
addOne :: Int -> Int
addOne x = x + 1

-- | Returns (fixed) version.
withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 0.1

-- | Returns (fixed) timestamp.
withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362

-- | Returns (fixed) OS.
withOS :: (String -> r) -> r
withOS f = f "linux"

-- | Return version, timestamp and OS as pyramid of doom.
releaseString :: String
releaseString =
  withOS $ \os ->
    withVersionNumber $ \version ->
      withTimestamp $ \timestamp ->
        os ++ "-v" ++ show version ++ "-" ++ show timestamp

-- | Return version, timestamp and OS using the 'CPS' type.
releaseStringCPS :: CPS String
releaseStringCPS = do
  os <- CPS withOS
  version <- CPS withVersionNumber
  timestamp <- CPS withTimestamp
  pure $ os ++ "-v" ++ show version ++ "-" ++ show timestamp
