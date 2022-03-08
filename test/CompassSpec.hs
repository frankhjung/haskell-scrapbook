{-# LANGUAGE ScopedTypeVariables #-}

module CompassSpec (spec) where

import           Compass    (Direction (..), Turn (..), cpred, csucc, every,
                             orientate, orientateMany, rotate, rotateMany)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "test compass rotation" $
    it "no turn so direction unchanged" $
      all (\d -> rotate TNone d == d) [minBound .. maxBound] `shouldBe` True
  describe "test compass orientation" $ do
    it "no turn" $
      all (\d -> orientate d d == TNone) [minBound .. maxBound] `shouldBe` True
    it "turn left" $
      all (\d -> orientate d (cpred d) == TLeft) [minBound .. maxBound] `shouldBe` True
    it "turn right" $
      all (\d -> orientate d (csucc d) == TRight) [minBound .. maxBound] `shouldBe` True
    it "turn around" $
      all (\d -> orientate d (cpred (cpred d)) == TAround) [minBound .. maxBound] `shouldBe` True
  describe "turn around same as all turns " $
    it "many turns leading to same orientation" $
      all (\d -> rotateMany d every == rotate TAround d) every `shouldBe` True
  describe "many directions compared to one turn right" $
    it "all turn right" $
      orientateMany every (map csucc (every :: [Direction])) `shouldBe` replicate 4 TRight
