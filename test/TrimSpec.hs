{-# LANGUAGE ScopedTypeVariables #-}

module TrimSpec (spec) where

import           Trim                  (dropWhile', trim)

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSize, prop)
import           Test.QuickCheck       (Positive (..))

import           Data.Char             (isSpace)

-- | Used to pad test cases.
spaces :: Int -> String
spaces n = replicate n ' '

spec :: Spec
spec =
  describe "trim spaces from front" $ do
    it "trim' \" abc \" is \"abc\"" $
      trim " abc " `shouldBe` "abc"
    it "dropWhile' isSpace \" abc \" is \"abc \"" $
      dropWhile' isSpace " abc " `shouldBe` "abc "
    modifyMaxSize (const 10) $
      prop "dropWhile' same Prelude dropWhile" $
        \(xs :: String,  Positive (n :: Int)) ->
          dropWhile' isSpace (spaces n) <> xs == dropWhile isSpace (spaces n) <> xs
