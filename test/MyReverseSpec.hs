{-# LANGUAGE ScopedTypeVariables #-}

module MyReverseSpec (spec) where

import           MyReverse                 (myRevl, myRevr, myRevr2)
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck.Modifiers (NonEmptyList (NonEmpty))

spec :: Spec
spec =
  describe "last and penultimate of lists" $ do
    prop "head of myRevl is same as last" $
      \(NonEmpty (xs :: [Int])) -> (head . myRevl) xs == last xs
    prop "myRevl is same as myRevr" $
      \(xs :: [Int]) -> myRevl xs == myRevr xs
    prop "myRevr is same as myRevr2" $
      \(xs :: [Int]) -> myRevr xs == myRevr2 xs

