module HarmonicOscillationSpec (spec) where

import           HarmonicOscillation (HarmonicOscillation (..), harmonic)

import           Control.Monad.State (evalState)
import           Test.Hspec          (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec =
  describe "Harmonic Oscillation" $
    it "generates the correct positions" $ do
      let
        positions = take 10 $ evalState harmonic (HarmonicOscillation 0 0)
        expected  = take 10 $ map sin [0, 0.01..]
        epsilon   = 1e-9
      -- Compare the generated positions with the expected values this
      -- generates a list of got and expected positions and checks if
      -- they are within epsilon of each other.
      zip positions expected
        `shouldSatisfy` all (\(p, e) -> abs (p - e) < epsilon)
