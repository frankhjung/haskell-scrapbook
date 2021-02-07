{-# LANGUAGE ScopedTypeVariables #-}

module MultiplySpec (spec) where

import           Multiply              (multiply0, multiply1, multiply2,
                                        multiply3)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Positive (..))

spec :: Spec
spec =
  describe"multiplication algorithms for positive integers" $ do
    prop "multiply0" $
      \(Positive (n :: Int), Positive (a :: Int)) -> multiply0 n a == n * a
    prop "multiply1" $
      \(Positive (n :: Int), Positive (a :: Int)) -> multiply1 n a == n * a
    prop "multiply2" $
      \(Positive (n :: Int), Positive (a :: Int)) -> multiply2 n a == n * a
    prop "multiply3" $
      \(Positive (n :: Int), Positive (a :: Int)) -> multiply3 n a == n * a

