module WeekdaySpec (spec) where

import           Weekday               (Weekday (..), capitalise, fromString,
                                        fullWeek)

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Gen, elements, forAll, oneof, vectorOf)

-- | Returns a full list of week days as a strings.
weekdays :: [String]
weekdays = map show (fullWeek :: [Weekday])

-- | Choose a random element from weekdays.
weekdayString :: Gen String
weekdayString = oneof [return d | d <- weekdays]

-- | Produce a lowercase string of 4 characters.
random4String :: Gen String
random4String = vectorOf 4 $ elements ['a'..'z']

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
    it "captilised mON is Mon" $
      capitalise "mON" `shouldBe` "Mon"
    it "captilised Tue is Tue" $
      capitalise "Tue" `shouldBe` "Tue"
    it "from string sun is Just Sun" $
      fromString "sun" `shouldBe` Just Sun
    it "from string bad is Nothing" $
      fromString "bad" `shouldBe` Nothing
    it "show of read from weekday string returns weekday string" $
      forAll weekdayString $ \d -> show (read d :: Weekday) `shouldBe` d
    it "invalid weekdays" $
      forAll random4String $ \d -> fromString d `shouldBe` Nothing
    -- use weekday generator
    prop "read from show Weekday returns Weekday" $
      \d -> read (show d) `shouldBe` (d :: Weekday)
    -- a better way to do the same as above
    prop "property read from show Weekday returns Weekday"
      prop_ReadShowWeekday
