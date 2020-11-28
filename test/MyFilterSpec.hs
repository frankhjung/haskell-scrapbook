{-# LANGUAGE ScopedTypeVariables #-}

module MyFilterSpec (spec) where

import           MyFilter              (myFilter)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)

-- | Predicate for filter test.
p :: Int -> Bool
p = (>3)

spec :: Spec
spec =
  describe "myFilter same as preludes filter" $
    prop "myFilter same as filter for potentially empty lists" $
      \(xs :: [Int]) -> myFilter p xs == filter p xs
