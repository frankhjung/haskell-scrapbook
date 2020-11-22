{-# LANGUAGE ScopedTypeVariables #-}

module CpsSpec (spec) where

import           Control.Monad.Trans.Cont (runCont)
import           Cps                      (pythagorasCont)
import           Test.Hspec               (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck    (prop)

spec :: Spec
spec =
  describe "continuation passing style - cps" $ do
    it "pythagoras 3 4 is 25" $
      runCont (pythagorasCont 3 4) Just `shouldBe` Just 25
    prop "pythagoras a b is a*a + b*b" $
      \(a :: Int, b :: Int) -> runCont (pythagorasCont a b) Just == Just (a*a + b*b)
