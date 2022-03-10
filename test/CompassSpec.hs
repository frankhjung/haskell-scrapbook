{-# LANGUAGE ScopedTypeVariables #-}

module CompassSpec (spec) where

import           Compass       (Direction (..), Turn (..), cpred, csucc, every,
                                orientate, orientateMany, rotate, rotateMany,
                                rotateManyTurns)
import           Control.Monad (ap)
import           Test.Hspec    (Spec, describe, it, shouldBe)

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
      all (ap ((==) . flip rotateMany every) (rotate TAround)) every `shouldBe` True
  describe "many directions compared to one turn right" $
    it "all turn right" $
      orientateMany every (map csucc (every :: [Direction])) `shouldBe` replicate 4 TRight
  describe "apply many turns in succession to a direction" $
    it "all return to start direction" $
      fmap (last . flip rotateManyTurns (TAround : every)) every `shouldBe` every
  describe "check monoid laws" $ do
    it "identity" $
      mempty `shouldBe` TNone
    it "left identity" $
      all (\d -> (d <> TNone ) == d) ([minBound .. maxBound] :: [Turn]) `shouldBe` True
    it "right identity" $
      all (\d -> (TNone <> d) == d) ([minBound .. maxBound] :: [Turn]) `shouldBe` True
    it "associativity" $
      all (\d -> (d <> (d <> d)) == (d <> d) <> d) ([minBound .. maxBound] :: [Turn]) `shouldBe` True
