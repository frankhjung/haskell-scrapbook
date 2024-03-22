module Main ( main
            , playGame
            , startState
            -- * Types
            , GameOn
            , GameValue
            , GameState
            ) where

import           Control.Monad.State (State, evalState, gets, modify)
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

type GameOn = Bool                          -- game on/off
type GameValue = Int                        -- game value
type GameState = (GameOn, GameValue)        -- state, score

-- | Game is off by default.
off :: GameOn
off = False

-- | Game is off by default.
startState :: GameState
startState = (off, 0)

-- | Toggle game state on/off.
toggleGame :: GameOn -> GameOn
toggleGame = not

-- | Play game given some input string of instructions.
playGame :: String                          -- game input
            -> State GameState GameValue    -- new state
playGame []     = gets snd                  -- empty input, return score
playGame (x:xs) = do                        -- process game input
    modify $ \(switch, score) -> case x of  -- update state and score
        'a' | switch -> (switch, score + 1) -- a gives +1
        'b' | switch -> (switch, score - 1) -- b gives -1
        'c'          -> (toggleGame switch, score)   -- c toggles game on/off
        _            -> (switch, score)     -- no-op
    playGame xs                             -- continue game

-- | Play game read from command line.
--
-- === Example
--
-- "abcaaacbbcabbab" result = 2, as (5 - 3 = 2) as game is initially off
-- @
-- $ stack exec -- stategame "abcaaacbbcabbab"
-- 2
-- $ stack exec -- stategame "ca"
-- 1
-- $ stack exec -- stategame "cb"
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
