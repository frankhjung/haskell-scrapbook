module Main ( main
            , playGame
            , startState
            -- * Types
            , GameValue
            , GameState
            ) where

import           Control.Monad.State (State, evalState, get, put)
import           System.Environment  (getArgs)

-- == Example use of State monad
--
-- Passes a string of dictionary {a,b,c}
-- Game is to produce a number from the string.
-- By default the game is off, a C toggles the
-- game on and off. A 'a' gives +1 and a b gives -1.
--
-- === Example
--
-- 'ab'    = 0
-- 'ca'    = 1
-- 'cabca' = 0
--
-- State = game is on or off & current score
--       = (Bool, Int)
--
-- Source: https://wiki.haskell.org/State_Monad

type GameValue = Int
type GameState = (Bool, Int)

-- | Game is off by default.
startState :: (Bool, Int)
startState = (False, 0)

-- | Play game given
playGame :: String                        -- Game input
            -> State GameState GameValue  -- initial State
-- | Empty
playGame []     = do
    (_, score) <- get
    return score
-- | Process game input
playGame (x:xs) = do
    (on, score) <- get
    case x of
         'a' | on -> put (on, score + 1)
         'b' | on -> put (on, score - 1)
         'c'      -> put (not on, score)
         _        -> put (on, score)
    playGame xs

-- | Play game read from command line.
--
-- === Example
--
-- "abcaaacbbcabbab" result = 2, as (5 - 3 = 2) as game not initially on
-- @
-- $ stack exec -- stategame "ca"
-- 1
-- [~/dev/haskell/scrapbook(master *+)]$ stack exec -- stategame "cb"
-- -1
-- @
--
-- === Notes
--
-- @
-- λ> :t evalState
-- evalState :: State s a -> s -> a
-- @
main :: IO ()
main = print . flip evalState startState . playGame . head =<< getArgs
