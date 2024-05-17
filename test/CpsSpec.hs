{-# LANGUAGE ScopedTypeVariables #-}

module CpsSpec (spec) where

import           Control.Monad.Trans.Cont (runCont)
import           Cps                      (fromCPS, pythagorasCont, toCPS)
import           Test.Hspec               (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck    (prop)

spec :: Spec
spec = do
  describe "continuation passing style (cps)" $ do
    it "pythagoras 3 4 is 25" $
      runCont (pythagorasCont 3 4) id `shouldBe` 25
    prop "pythagoras a b as Just" $
      \(a :: Int, b :: Int) -> runCont (pythagorasCont a b) Just == Just (a*a + b*b)
    prop "pythagoras a b as String" $
      \(a :: Int, b :: Int) -> runCont (pythagorasCont a b) show == show (a*a + b*b)
  describe "custom cps" $
    prop "fromCPS to id" $
      \(a :: Int) -> fromCPS (toCPS a) `shouldBe` a
