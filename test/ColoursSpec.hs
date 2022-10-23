{-# LANGUAGE ScopedTypeVariables #-}

module ColoursSpec (spec) where

import           Colours               (Colour (..))
import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)

spec :: Spec
spec =
  describe "test semigroup and monoid using colours" $ do
    it "green & green is green" $
      Green <> Green `shouldBe` Green
    it "red & blue is purple" $
      Red <> Blue `shouldBe` Purple
    it "blue & red is purple" $
      Red <> Blue `shouldBe` Purple
    it "blue & orange is brown" $
      Blue <> Orange `shouldBe` Brown
    it "orange & blue is brown" $
      Orange <> Blue `shouldBe` Brown
    it "(green <> blue) <> yellow same as green <> (blue <> yellow)" $
      (Green <> Blue) <> Yellow `shouldBe` Green <> Blue <> Yellow
    prop "colours match regardless of order" $
      \(a :: Colour, b :: Colour, c:: Colour) -> (a <> b) <> c `shouldBe` a <> b <> c
