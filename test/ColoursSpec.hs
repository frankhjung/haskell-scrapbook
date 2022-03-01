{-# LANGUAGE ScopedTypeVariables #-}

module ColoursSpec (spec) where

import           Colours    (Colour (..))
import           Test.Hspec (Spec, describe, it, shouldBe)

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
    it "(Green <> Blue) <> Yellow same as Green <> (Blue <> Yellow)" $
      (Green <> Blue) <> Yellow `shouldBe` Green <> (Blue <> Yellow)
