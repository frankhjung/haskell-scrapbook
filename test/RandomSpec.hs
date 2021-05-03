{-# LANGUAGE ScopedTypeVariables #-}

module RandomSpec (spec) where

import           Data.Ix                 (inRange)
import           Random                  (dice, roll)
import           Test.Hspec              (Spec, describe)
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck         (Positive (..))
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

spec :: Spec
spec =
  describe "random with seed" $
    prop "dice rolls are bounded in range 1..6" $
      \(Positive (n :: Int)) -> monadicIO $ do
        ns <- run (roll n)
        assert (all isBounded ns)
        where
          isBounded = inRange dice
