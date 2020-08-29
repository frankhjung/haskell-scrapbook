module StackSpec (spec) where

import           Stack      (Stack, empty, tasks)
import           State      (evalState, execState)

import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "state using a stack" $ do
    it "evalState is 5" $
      evalState tasks empty `shouldBe` 5
    it "execState is [5,1]" $
      execState tasks empty `shouldBe` ([5,1] :: Stack)
