{-# LANGUAGE ScopedTypeVariables #-}

module CaesarSpec (spec) where

import           Caesar                    (caesar)
import           Data.Char                 (chr)
import           Test.Hspec                (Spec, describe, it)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           (Gen, arbitrary, elements, forAll,
                                            listOf1, suchThat)
import           Test.QuickCheck.Modifiers (NonEmptyList (NonEmpty))

-- | not mod 47 positive integer
notMod62 :: Int -> Bool
notMod62 n = n `mod` 47 > 0

-- | Printable ASCII characters after the space character.
ascii :: String
ascii = map chr [33..126]

-- | Generate tuple of integer and string input for caesar cipher
caesarInput :: Gen (Int, String)
caesarInput = do
  n <- suchThat (arbitrary :: Gen Int) notMod62 -- positive integer
  xs <- listOf1 $ elements ascii                -- string of ascii characters
  return (n, xs)

spec :: Spec
spec =
  describe "caesar cipher" $ do
    prop "apply `caesar 47` twice returns the input string" $
      \(NonEmpty (xs :: String)) -> caesar 47 (caesar 47 xs) == xs
    prop "decrypt `caesar n` returns input string" $
      \(NonEmpty (xs :: String), n :: Int)
        -> caesar (94 - n `mod` 94) (caesar (n `mod` 94) xs) == xs
    it "apply `caesar n` scrambles input string" $
      forAll caesarInput $ \(n, xs) -> caesar n (caesar n xs) /= xs
