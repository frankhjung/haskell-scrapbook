{-|

Module      : WeekDay
Description : Enumerate week days
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Weekday ( Weekday(..)
               , fromString
               , fullWeek
               , capitalised
               ) where

import           Data.Char       (toLower, toUpper)
import           Test.QuickCheck (Arbitrary (arbitrary), oneof)

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
                deriving (Eq, Enum, Show, Bounded, Read)

-- | Provide an random 'Weekday'.
-- λ> generate arbitrary :: IO Weekday
-- Wed
instance Arbitrary Weekday where
  arbitrary = oneof [return d | d <- fullWeek :: [Weekday]]

-- λ> toEnum 0 :: WeekDay
-- Mon

-- Read a Weekday from a string.
--
-- λ> fromString "monday"
-- Mon
-- λ> fromString "mon"
-- Mon
-- λ> fromString "Mon"
-- Mon
fromString :: String -> Weekday
fromString = read . take 3 . capitalised

-- λ> fullWeek :: [Weekday]
-- [Mon,Tue,Wed,Thu,Fri,Sat,Sun]
fullWeek :: (Bounded a, Enum a) => [a]
fullWeek = enumFromTo minBound maxBound

-- Return title case of string.
-- λ> capitalised "monday"
-- "Monday"
capitalised :: String -> String
capitalised [] = []
capitalised x  = toUpper (head x) : map toLower (tail x)
