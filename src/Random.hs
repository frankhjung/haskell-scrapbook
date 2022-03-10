{-|

Module      : Random
Description : Test random generators
Copyright   : © Frank Jung, 2020
License     : GPL-3

Exploration of the use of 'System.Random' inspired by
<https://www.packtpub.com/product/haskell-design-patterns/9781783988723 Haskell Design Patterns by Ryan Lemmer>

== Example

Simulate roll a fair 6-sided dice /n/ times.

With a 'seed' of @111111@ then we expect @[4,6,5,3,2]@.

-}

module Random (dice, roll, rolls, seed) where

import           Control.Monad (replicateM)
import           System.Random (StdGen, mkStdGen, randomRIO, randomRs)

-- | Range for a 6-sided dice.
dice :: (Int, Int)
dice = (1, 6)

-- | Seed the dice's random generator.
seed :: Int           -- random seed
        -> StdGen     -- random generator
seed = mkStdGen

-- | Return /n/ rolls of the dice using the system random generator.
roll :: Int           -- number of rolls
        -> IO [Int]   -- list of dice rolls
roll n = replicateM n (randomRIO dice)

-- | Produce an infinite stream of 'dice' rolls.
rolls :: StdGen       -- random generator
         -> [Int]     -- stream of dice rolls
rolls = randomRs dice
