#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

{-|

  Module      : TurtleExample
  Description : Explore turtle.
  Copyright   : © Frank Jung, 2019-2020
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : education
  Portability : Linux

  Uses <https://hackage.haskell.org/package/turtle-1.0.0/docs/Turtle-Tutorial.html Turtle>.

-}

module TurtleExample (main) where

import           Turtle

-- | Create a list of tuples.
example :: Shell ()
example = do
    x <- select [1, 2]
    y <- select [3, 4]
    liftIO (print (x, y))

-- | Run example loop.
main :: IO ()
main = sh example
