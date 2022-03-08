{-# LANGUAGE ScopedTypeVariables #-}

module CompassSpec (spec) where

import           Compass    (Direction (..), Turn (..), cpred, csucc, every,
                             orient, rotate, rotateMany)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "test compass rotation" $
    it "no turn so direction unchanged" $
      all (\d -> rotate TNone d == d) [minBound .. maxBound] `shouldBe` True
  describe "test compass orientation" $ do
    it "no turn" $
      all (\d -> orient d d == TNone) [minBound .. maxBound] `shouldBe` True
    it "turn left" $
      all (\d -> orient d (cpred d) == TLeft) [minBound .. maxBound] `shouldBe` True
    it "turn right" $
      all (\d -> orient d (csucc d) == TRight) [minBound .. maxBound] `shouldBe` True
    it "turn around" $
      all (\d -> orient d (cpred (cpred d)) == TAround) [minBound .. maxBound] `shouldBe` True
  describe "test compass many turns" $
    it "many turns leading to same orientation" $
      all (\d -> rotateMany d every == rotate TAround d) every `shouldBe` True
