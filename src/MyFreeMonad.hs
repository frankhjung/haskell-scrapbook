{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

Module      : MyFreeMonad
Description : Implement a Free Monad using a DSL as an example.
Copyright   : © Frank Jung, 2023
License     : GPL-3

The code was originally provided by ChatGPT. It had to be modified to
compile. I added tests.

-}

module MyFreeMonad ( ArithM
                   , ArithF (..)
                   , addM
                   , subM
                   , mulM
                   , divM
                   , evalArith
                   , example
                   , example'
                   ) where

import           Control.Monad.Free (Free (..), liftF)

-- | Arithmetic functor.
data ArithF x = Add Int x | Sub Int x | Mul Int x | Div Int x deriving (Show, Functor)

-- | Arithmetic free monad.
type ArithM = Free ArithF

-- | Given Arithmetic free monad, return its value.
evalArith :: Free ArithF Int -> Int
evalArith (Free (Add x n)) = evalArith n + x
evalArith (Free (Sub x n)) = evalArith n - x
evalArith (Free (Mul x n)) = evalArith n * x
evalArith (Free (Div x n)) = evalArith n `div` x
evalArith (Pure x)         = x

addM :: Int -> ArithM ()
addM x = liftF (Add x ())

subM :: Int -> ArithM ()
subM x = liftF (Sub x ())

mulM :: Int -> ArithM ()
mulM x = liftF (Mul x ())

divM :: Int -> ArithM ()
divM x = liftF (Div x ())

-- | A simple example on how to use DSL:
--
-- @example 0 = ((((0+10)*2)-10)/2) == 5@
--
-- Get back the integer value with:
--
-- @evalArith (example 0)@
example :: Int -> ArithM Int
example n =
  divM 2
  >> subM 10
  >> mulM 2
  >> addM 10
  >> return n

-- | Another example on how to use this DSL:
example' :: Int -> ArithM Int
example' n =
  divM 2
  >> mulM 2
  >> return n
