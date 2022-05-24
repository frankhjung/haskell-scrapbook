{-# LANGUAGE ScopedTypeVariables #-}

module MyTakeSpec (spec) where

import           MyTake                (myTake)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)

spec :: Spec
spec =
  describe "myTake" $
    prop "myTake same as preludes take" $
      \(n :: Int, xs :: String) -> myTake n xs == take n xs
