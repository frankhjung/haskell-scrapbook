{-# LANGUAGE ScopedTypeVariables #-}

module ExprSpec (spec) where

import           Data.Bool             (bool)
import           Expr                  (Expr (..), eval)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)

spec :: Spec
spec =
  describe "evaluate" $ do
    prop "Num a is a" $
      \(a :: Int) -> eval (Num a) == a
    prop "Plus a b is a + b" $
      \(a :: Int, b :: Int) -> eval (Plus (Num a) (Num b)) == a + b
    prop "Eq a b is a = b" $
      \(a :: Int, b :: Int) -> eval (Eq (Num a) (Num b)) == (a == b)
    prop "If (Eq a b) is true then a else b" $
      \(a :: Int, b :: Int)
        -> eval (If (Eq (Num a) (Num b)) (Num a) (Num b)) == bool b a (a == b)
