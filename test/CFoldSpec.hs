{-# LANGUAGE ScopedTypeVariables #-}

module CFoldSpec (spec) where

import           CFold                 (cfold, cfold')
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)

spec :: Spec
spec =
  describe "continuation passing style - fold" $ do
    prop "cfold (+) 0 same as sum" $
      \(xs :: [Int]) -> cfold (+) 0 xs == sum xs
    prop "cfold (:) [] same as cons" $
      \(xs :: [Int]) -> cfold (:) [] xs == xs
    prop "cfold\' (\\x t g -> x : g t) [] same as cons" $
      \(xs :: [Int]) -> cfold' (\x t g -> x : g t) [] xs == xs
    prop "cfold\' (\\x t g -> g (x : t)) [] same as reverse" $
      \(xs :: [Int]) -> cfold' (\x t g -> g (x : t)) [] xs == reverse xs
