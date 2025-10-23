{-|

Module      : Fractions
Description : Use Haskell for fraction calculations
Copyright   : © Frank Jung, 2020
License     : GPL-3

-}

module Fractions (sumOfFractions) where

import           Data.Ratio (Ratio, (%))

-- | Add two fractions.
--
-- To try this in GHCi:
-- > import Data.Ratio
-- > let result = (28 % 50) + (26 % 50)
-- > result
-- 27 % 25
--
-- Or load this file and run:
-- > sumOfFractions
-- 27 % 25
sumOfFractions :: Ratio Integer
sumOfFractions = 28 % 50 + 26 % 50
