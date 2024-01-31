{-# LANGUAGE ScopedTypeVariables #-}
module WeekdaySpec (spec) where

import           Weekday               (Weekday (..), capitalise, fullWeek,
                                        makeWeekday)

import           Data.Char             (toTitle)
import           Data.Maybe            (isNothing)
import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Gen, elements, forAll, oneof, vectorOf)

-- | Returns a full list of week days as a strings.
weekdays :: [String]
weekdays = map show (fullWeek :: [Weekday])

-- | Choose a random element from weekdays.
weekdayString :: Gen String
weekdayString = oneof [return d | d <- weekdays]

-- Property string is title case.
propIsCapitalised :: String -> Bool
propIsCapitalised []       = null (capitalise [])
propIsCapitalised xs@(x:_) = (head . capitalise) xs == toTitle x

-- | Produce a lowercase string of 4 characters.
random4String :: Gen String
random4String = vectorOf 4 $ elements ['a'..'z']

-- Property that string never returns 'Weekday'.
propNotWeekday :: String -> Bool
propNotWeekday = isNothing . makeWeekday

-- Property that show then read gives back 'Weekday'.
propReadShowWeekday :: Weekday -> Bool
propReadShowWeekday d = read (show d) == d -- pointfree (==) =<< read . show

-- | Unit Tests for Weekday
spec :: Spec
spec =
  describe "test weekday type" $ do
    it "fullWeek is 7 days" $
      length (fullWeek :: [Weekday]) `shouldBe` 7
    it "fullWeek first day is Mon" $
      head (fullWeek :: [Weekday]) `shouldBe` Mon
    it "fullWeek last day is Sun" $
      last (fullWeek :: [Weekday]) `shouldBe` Sun
    it "capitalised mON is Mon" $
      capitalise "mON" `shouldBe` "Mon"
    it "capitalised Tue is Tue" $
      capitalise "Tue" `shouldBe` "Tue"
    prop "head of string is capitalised"
      propIsCapitalised
    it "from string sun is Just Sun" $
      makeWeekday "sun" `shouldBe` Just Sun
    it "from string bad is Nothing" $
      makeWeekday "bad" `shouldBe` Nothing
    it "show of read from weekday string returns weekday string" $
      forAll weekdayString $ \d -> show (read d :: Weekday) `shouldBe` d
    -- never a valid weekday
    it "invalid weekdays" $
      forAll random4String $ \d -> makeWeekday d `shouldBe` Nothing
    -- ... a better way to do this
    prop "property invalid weekdays" $
      forAll random4String propNotWeekday
    -- use weekday generator
    prop "read from show Weekday returns Weekday" $
      \(d :: Weekday) -> read (show d) `shouldBe` d
    -- ... a better way to do this
    prop "property read from show Weekday returns Weekday"
      propReadShowWeekday
