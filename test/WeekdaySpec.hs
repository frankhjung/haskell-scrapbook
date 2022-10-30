{-# LANGUAGE ScopedTypeVariables #-}
module WeekdaySpec (spec) where

import           Weekday               (Weekday (..), capitalise, fromString,
                                        fullWeek)

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

-- | Produce a lowercase string of 4 characters.
random4String :: Gen String
random4String = vectorOf 4 $ elements ['a'..'z']

-- Property that string never returns 'Weekday'.
prop_Not_Weekday :: String -> Bool
prop_Not_Weekday = isNothing . fromString

-- Property that show then read gives back 'Weekday'.
prop_Read_Show_Weekday :: Weekday -> Bool
prop_Read_Show_Weekday d = read (show d) == d -- pointfree (==) =<< read . show

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
    -- never a valid weekday
    it "invalid weekdays" $
      forAll random4String $ \d -> fromString d `shouldBe` Nothing
    -- ... a better way to do this
    prop "property invalid weekdays" $
      forAll random4String prop_Not_Weekday
    -- use weekday generator
    prop "read from show Weekday returns Weekday" $
      \(d :: Weekday) -> read (show d) `shouldBe` d
    -- ... a better way to do this
    prop "property read from show Weekday returns Weekday"
      prop_Read_Show_Weekday
