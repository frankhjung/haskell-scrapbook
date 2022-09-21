{-# LANGUAGE ScopedTypeVariables #-}

module CaesarSpec (spec) where

import           Caesar                    (caesar, digits, indexOf, isDigit,
                                            isLower, isMisc, isUpper,
                                            lowerAlphabet, rot13, rot135,
                                            upperAlphabet)
import           Test.Hspec                (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck.Modifiers (NonEmptyList (NonEmpty))

spec :: Spec
spec =
  describe "caesar cipher" $ do
    it "have lowercase characters" $
      all isLower lowerAlphabet `shouldBe` True
    it "do not have lowercase characters" $
      all isLower (digits ++ upperAlphabet) `shouldBe` False
    it "have uppercase characters" $
      all isUpper upperAlphabet `shouldBe` True
    it "do not have uppercase characters" $
      all isUpper (digits ++ lowerAlphabet) `shouldBe` False
    it "have digits" $
      all isDigit digits `shouldBe` True
    it "do not have digits" $
      all isDigit (digits ++ lowerAlphabet ++ upperAlphabet) `shouldBe` False
    it "have miscellaneous characters" $
      all isMisc ".!@#%^&*_-+{}[]():';,/?<>=" `shouldBe` True
    it "do no have miscellaneous characters" $
      all isMisc (digits ++ lowerAlphabet ++ upperAlphabet) `shouldBe` False
    it "found index" $
      map (`indexOf` "abc") "abc" `shouldBe` [0,1,2]
    prop "apply caesar 13 twice returns the input string" $
      \(NonEmpty (xs :: String)) -> caesar 13 (caesar 13 xs) == xs
    prop "apply rot13 twice returns the input string" $
      \(NonEmpty (xs :: String)) -> rot13 (rot13 xs) == xs
    prop "apply rot135 twice returns the input string" $
      \(NonEmpty (xs :: String)) -> rot135 (rot135 xs) == xs
