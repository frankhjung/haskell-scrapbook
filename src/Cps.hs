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
  ( -- * Functions
    addCont
  , pythagorasCont
  , squareCont
  , toCPS
  , fromCPS
  ) where

import           Control.Monad.Trans.Cont (Cont, callCC)

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

-- | Custom continuation passing style.
--
-- The following example uses a custom continuation passing style (CPS) to
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
