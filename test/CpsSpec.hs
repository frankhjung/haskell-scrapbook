{-# LANGUAGE ScopedTypeVariables #-}

module CpsSpec (spec) where

import           Control.Monad.Trans.Cont (runCont)
import           Cps                      (CPS (..), addOne, fromCPS,
                                           pythagorasCont, releaseString,
                                           releaseStringCPS, toCPS)
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
      \(as :: String) -> fromCPS (toCPS as) `shouldBe` as
  describe "using type CPS" $ do
    prop "CPS as Functor" $ ------------- CPS value ----------------------
      \(a :: Int) -> runCPS (fmap addOne (CPS ($ a))) id `shouldBe` succ a
    it "releaseString" $
      releaseString `shouldBe` "linux-v0.1-1532083362"
    it "releaseStringCPS" $
      releaseStringCPS `shouldBe` "linux-v0.1-1532083362"
