{-|

Module      : WeekDay
Description : Enumerate week days
Copyright   : © Frank Jung, 2022
License     : GPL-3

A simple demonstration on how to map to and from a bounded, enumerated data
type.

In this example we use the weekdays names as our type to iterate across.

  * 'makeWeekday' is a simple conversion function to map a 'Weekday'
     from a string
  * 'fullWeek' returns a list of all abbreviated weekdays
  * 'capitalise' returns the title case of a string

The 'makeWeekday' code is rather weak as any word where the first 3 letters
map to a weekday is valid for conversion.

== Other Examples

=== Using enum:

This example shows how use 'Weekday' from an enumeration:

>>> toEnum 0 :: WeekDay
Mon

-}

module Weekday ( Weekday(..)
               , makeWeekday
               , fullWeek
               , capitalise
               ) where

import           Data.Char       (toLower, toTitle)
import           Test.QuickCheck (Arbitrary (arbitrary), arbitraryBoundedEnum)
import           Text.Read       (readMaybe)

-- | Abbreviated days of the week from Mon (Monday) to Sun (Sunday).
data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
                deriving (Eq, Enum, Show, Bounded, Read)

-- | Provide a random instance of a 'Weekday'.
--
-- This is for ease of use in property testing.
--
-- >>> generate arbitrary :: IO Weekday
-- Wed
instance Arbitrary Weekday where
  arbitrary = arbitraryBoundedEnum

-- | Read a 'Weekday' from a String.
--
-- Some string to 'Weekday' examples:
--
-- >>> makeWeekday "tHU"
-- Just Thu
--
-- >>> makeWeekday "sun"
-- Just Sun
--
-- >>> makeWeekday "Bad"
-- Nothing
--
-- Any string bigger than 3 characters also fails:
-- >>> makeWeekday "Mond"
-- Nothing
makeWeekday :: String -> Maybe Weekday
makeWeekday = readMaybe . capitalise . take 4

-- | List all days of the week.
--
-- >>> fullWeek
-- [Mon,Tue,Wed,Thu,Fri,Sat,Sun]
fullWeek :: [Weekday]
fullWeek = [minBound..maxBound]

-- | Return title case of string.
--
-- >>> capitalise "monday"
-- "Monday"
--
-- >>> null (capitalise "")
-- True
capitalise :: String -> String
capitalise = zipWith id (toTitle : repeat toLower)
