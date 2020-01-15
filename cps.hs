#!/usr/bin/runhaskell
{-|

Module      : Cps
Description : Continuation passing style, examples.
Copyright   : © Frank Jung, 2020
License     : GPL-3

Example from <https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style Wikibooks Continuation Passing Style>.

Uses the Cont monad from the transformers package.

Using callCC is better than using return as argument is a function and it
returns a suspended computation.

Call `main` to run example.

-}

module Cps (main, addCont, squareCont, pythagorasCont) where

import           Control.Monad.Trans.Cont (Cont, callCC, runCont)

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

-- | Example call to continuation.
example :: [Int] -> String
example (a:b:_) = runCont (pythagorasCont a b) show
example []      = "Nothing"
example _       = "Nothing"

-- | Run example.
--
-- >>> echo 3 4 | runhaskell cps.hs
-- 25
--
main :: IO ()
main = interact $ example . map read . words
