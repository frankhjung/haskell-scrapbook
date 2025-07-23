{-|

Module      : HarmonicStep
Description : A module to simulate harmonic osciallation.
Copyright   : © Frank Jung, 2025
License     : GPL-3

This Haskell code defines a module to simulate simple
<https://en.wikipedia.org/wiki/Harmonic_oscillator harmonic oscillation>, like
the motion of a pendulum. It does this using a 'State' monad to manage the
changing position and time.

== Example usage

To compute the position of a harmonic oscillator:

@
let positions = evalState harmonic (HarmonicOscillation 0 0)
take 10 positions
@

[@evalState harmonic (HarmonicOscillation 0 0)@] This runs the harmonic
computation, starting with an initial state where position is 0 and time is 0.

[@evalState@] discards the final state and gives you just the result, which is
an infinite list of positions.

[@take 10 positions@] Since positions is an infinite list, you use a function
like 'take' to get a finite number of elements from it.

-}

module HarmonicOscillation (
    HarmonicOscillation(..),
    harmonic,
    step,
) where

import           Control.Monad.State (State, get, put)

-- | Represents the state of a harmonic oscillator.
-- This is a simple record that holds the state of the oscillator at any given
-- moment: its current position and the current time.
data HarmonicOscillation = HarmonicOscillation
    { position :: Double  -- ^ Position of the oscillator
    , time     :: Double  -- ^ Time of the oscillator (0.01 second increments)
    }

-- | Step function to update the state of the harmonic oscillator.
-- It calculates the new position based on the sine function and increments
-- the time. Returns the new position.
step :: State HarmonicOscillation Double
step = do
    (HarmonicOscillation _ t) <- get
    let p' = sin t
    put (HarmonicOscillation p' (t + 0.01))
    return p'

-- | This function generates an infinite list of positions over time.
harmonic :: State HarmonicOscillation [Double]
harmonic = do
    p <- step
    ps <- harmonic
    return (p : ps)
