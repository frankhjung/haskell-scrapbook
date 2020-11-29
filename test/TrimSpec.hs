{-# LANGUAGE ScopedTypeVariables #-}

module TrimSpec (spec) where

import           Trim                  (dropWhile', dropWhileEnd', trim,
                                        trimEnd, trimStart)

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSize, prop)
import           Test.QuickCheck       (Positive (..))

import           Data.Char             (isSpace)
import           Data.List             (dropWhileEnd)

-- | Used to pad test cases.
spaces :: Int -> String
spaces n = replicate n ' '

spec :: Spec
spec =
  describe "trim spaces from string" $ do
    it "trimStart \" abc \" is \"abc \"" $
      trimStart " abc " `shouldBe` "abc "
    it "trimEnd \" abc \" is \" abc\"" $
      trimEnd " abc " `shouldBe` " abc"
    it "trim \" abc \" is \"abc\"" $
      trim " abc " `shouldBe` "abc"
    modifyMaxSize (const 10) $
      prop "dropWhile' same Prelude dropWhile" $
        \(xs :: String,  Positive (n :: Int)) ->
          dropWhile' isSpace (spaces n <> xs) == dropWhile isSpace (spaces n <> xs)
    modifyMaxSize (const 10) $
      prop "dropWhileEnd' same Data.List dropWhileEnd" $
        \(xs :: String,  Positive (n :: Int)) ->
          dropWhileEnd' isSpace (xs <> spaces n) == dropWhileEnd isSpace (xs <> spaces n)
