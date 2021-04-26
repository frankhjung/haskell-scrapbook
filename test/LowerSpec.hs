module LowerSpec (spec) where

{-

  +m + Data.Char (chr)
  λ> (chr 97, chr 122)
  ('a','z')

  λ> (chr 65, chr 90)
  ('A','Z')

  -- | Generate a random ISO-8859-1 (8-bit) char.
  -- From package: random-1.2.0
  randomChar8 :: IO Char
  randomChar8 = getStdRandom $ randomR (chr 0,chr 255)

  <http://www.randomhacks.net.s3-website-us-east-1.amazonaws.com/2007/03/10/haskell-8-ways-to-report-errors/ 8 Ways to Report Errors in Haskell>

 -}

import           Lower           (mkLower)

import           Data.Char       (isUpper)
import           Data.Either     (fromLeft, fromRight, isLeft, isRight)
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Gen, arbitrary, elements, forAll, suchThat)

-- | Generate only lowercase letters
genLowerAlpha :: Gen Char
genLowerAlpha = elements ['a'..'z']

-- | Generate only non-lowercase characters.
genInvalidLower :: Gen Char
genInvalidLower = suchThat (arbitrary :: Gen Char) isUpper

spec :: Spec
spec =
  describe "check Lower constructors" $ do
    it "valid lowercase letters" $
      forAll genLowerAlpha $ isRight . mkLower
    it "is lowercase letter" $
      forAll genLowerAlpha $ \c -> either (const False) (const True) (mkLower c)
    it "invalid character" $
      forAll genInvalidLower $ isLeft . mkLower
    it "give error message" $
      forAll genInvalidLower $ \c -> fromLeft "expected error" (mkLower c) == "Not lowercase"
