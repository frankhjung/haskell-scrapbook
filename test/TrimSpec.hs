{-# LANGUAGE ScopedTypeVariables #-}

module TrimSpec (spec) where

import           Trim                  (dropWhile', trim)

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSize, prop)

import           Data.Char             (isSpace)

spec :: Spec
spec =
  describe "trim spaces from front" $ do
    it "trim' \" abc \" is \"abc\"" $
      trim " abc " `shouldBe` "abc"
    it "dropWhile' isSpace \" abc \" is \"abc \"" $
      dropWhile' isSpace " abc " `shouldBe` "abc "
    modifyMaxSize (const 10) $
      prop "dropWhile' same Prelude dropWhile" $
        \(xs :: String,  n :: Int) ->
          dropWhile' isSpace (replicate n ' ') <> xs == dropWhile isSpace (replicate n ' ') <> xs

