module YahtzeeSpec (spec) where

import           Yahtzee    (DiceChoice (..), DiceVals, allRolls)

import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "test yahtzee from given start point" $
    it "example game" $ do
      let diceVals = [Reroll, Keep 4, Keep 4, Reroll, Reroll]
      last (allRolls diceVals) `shouldBe` ([6, 4, 4, 6, 6] :: DiceVals)
