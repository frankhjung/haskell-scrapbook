{-# LANGUAGE ScopedTypeVariables #-}

module MyReverseSpec (spec) where

import           MyReverse                 (myRevRec, myRevl, myRevr)
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck.Modifiers (NonEmptyList (NonEmpty))

spec :: Spec
spec =
  describe "reversing lists" $ do
    prop "reverse twice same as input" $  -- reverse list twice
      \(xs :: [Int]) -> (myRevl . myRevl) xs == xs
    prop "myRevl same as myRevr" $        -- for non-trivial lists
      \(NonEmpty (xs :: [Int])) -> myRevl xs == myRevr xs
    prop "myRevr same as myRevRec" $       -- for non-trivial lists
      \(NonEmpty (xs :: [Int])) -> myRevr xs == myRevRec xs

