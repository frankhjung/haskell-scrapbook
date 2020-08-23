#!/usr/bin/env runhaskell

{-

Perform 'myAction' until user enters @q@ in response.

Haskell's ability to work with IO actions as with any other (functional and
non-functional) values allows us to define control structures of arbitrary
complexity.

Try, for example, to define a control structure that repeats an action
until it returns the 'False' result:

@
while :: IO Bool -> IO ()
while action = ???
@

Most programming languages don't allow you to define control structures at
all, and those that do often require you to use a macro-expansion system.
In Haskell, control structures are just trivial functions anyone can write.

Source: <https://wiki.haskell.org/IO_inside What is a monad?>

-}

module Main (bool, while, main, myAction) where

-- | Replace f with a function.
bool :: a       -- ^ value if false
        -> a    -- ^ value if true
        -> Bool -- ^ propostion being tested
        -> a    -- ^ the final value
bool f _ False = f
bool _ t True  = t

-- | Introduce function for a 'while' loop.
-- Perform @action@ until it returns @IO False@
while :: IO Bool -> IO ()
while action = do
  p <- action
  bool (pure ()) (while action) p

-- | My example action.
-- Get's user input.
-- Returns @False@ if the response contains a @q@.
-- Returns @True@ otherwise.
myAction :: IO Bool
myAction = do
  putStrLn "enter q to quit ..."
  s <- getLine
  putStrLn $ "Got " <> s
  bool (return True) (return False) ('q' `elem` s)

-- | Run 'myAction' until @q@.
--
-- >>> runghc app/While.hs
main :: IO ()
main = while myAction
