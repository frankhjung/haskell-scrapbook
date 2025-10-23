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

-- == Using the State monad
--
-- This game is to produce an integer from an input string given by the user.
-- The input string is a list of actions: 'a', 'b', 'c'.
--
-- By default the game is off:
--
-- * a 'c' toggles the game on and off
--
-- * a 'a' gives +1
--
-- * a 'b' gives -1
--
-- This Haskell code defines a simple game with a state. The game state is
-- represented by a tuple 'GameState' that includes a boolean 'GameOn' to
-- indicate if the game is on or off, and an integer 'GameValue' to keep
-- track of the game score.
--
-- The game starts with the startState function, which initializes the game
-- to be off (off = False) and the score to be 0.
--
-- The 'toggleGame' function is used to switch the game state on or off. It
-- takes the current game state as input and returns the opposite state.
--
-- The main logic of the game is in the 'playGame' function. This function
-- takes a string of instructions as input and returns a new game state.
-- The function uses pattern matching to process the input string:
--
-- * If the input string is empty ([]), the function returns the current score.
--
-- * If the input string starts with a character 'x' followed by the rest of
--   the string 'xs', the function updates the game state and score based on
--   the character 'x':
--
--      * If x is 'a' and the game is on, the score is increased by 1.
--
--      * If x is 'b' and the game is on, the score is decreased by 1.
--
--      * If x is 'c', the game state is toggled on or off.
--
--      * Any other character is a no-op and does not change the game state
--        or score.
--
-- After updating the game state and score, the function recursively calls
-- itself with the rest of the input string 'xs' to continue the game.
--
-- The main function reads the game instructions from the command line,
-- plays the game with these instructions using the 'playGame' function, and
-- then prints the final score. The 'evalState' function is used to run the
-- stateful computation of 'playGame' with the initial game state 'startState'.
--
-- === Example
--
-- 'ab'    = 0 'ca'    = 1 'cabca' = 0
--
-- State = game is on or off & current score = (Bool, Int)
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
        'a' | switch -> (switch, score + 1) -- a increases score
        'b' | switch -> (switch, score - 1) -- b decreases score
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
