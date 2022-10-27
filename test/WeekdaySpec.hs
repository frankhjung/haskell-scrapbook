module WeekdaySpec (spec) where

import           Weekday               (Weekday (..), capitalised, fromString,
                                        fullWeek)

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Gen, forAll, oneof)

-- | Returns a full list of week days as a strings.
weekdays :: [String]
weekdays = map show (fullWeek :: [Weekday])

-- | Choose a random element from weekdays.
weekdayString :: Gen String
weekdayString = oneof [return d | d <- weekdays]

prop_ReadShowWeekday :: Weekday -> Bool
prop_ReadShowWeekday d = read (show d) == d

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
    it "captilised mon is Mon" $
      capitalised "mon" `shouldBe` "Mon"
    it "captilised Mon is Mon" $
      capitalised "Mon" `shouldBe` "Mon"
    it "from string mon is Mon" $
      fromString "mon" `shouldBe` Mon
    it "from string monday is Mon" $
      fromString "monday" `shouldBe` Mon
    it "read from weekday string gives weekday" $
      forAll weekdayString $ \d -> show (read d :: Weekday) `shouldBe` d
    prop "read from show gives Weekday" $
      \d -> read (show d) `shouldBe` (d :: Weekday)
    prop "property read from show gives Weekday" prop_ReadShowWeekday
