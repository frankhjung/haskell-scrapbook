module CFoldSpec (spec) where

import           CFold           (cfold, cfold')
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck

-- | Continuation Passing Style cfold (+) 0 == sum
prop_cfold_sum :: [Int] -> Bool
prop_cfold_sum xs =  cfold (+) 0 xs == sum xs

-- | Continuation Passing Style cfold (:) [] == cons
prop_cfold_cons :: [Int] -> Bool
prop_cfold_cons xs =  cfold (:) [] xs == xs

-- | Continuation Passing Style cfold' (\x t g -> x : g t) [] == cons
prop_cfold'_cons :: [Int] -> Bool
prop_cfold'_cons xs = cfold' (\x t g -> x : g t) [] xs == xs

-- | Continuation Passing Style cfold' (\x t g -> g (x : t)) [] == reverse
prop_cfold'_rev :: [Int] -> Bool
prop_cfold'_rev xs = cfold' (\x t g -> g (x : t)) [] xs == reverse xs

spec :: Spec
spec =
  describe "continuation passing style - fold" $ do
    it "cfold (+) 0 same as sum" $
      quickCheck prop_cfold_sum
    it "cfold (:) [] same as cons" $
      quickCheck prop_cfold_cons
    it "cfold\' (\\x t g -> x : g t) [] same as cons" $
      quickCheck prop_cfold'_cons
    it "cfold\' (\\x t g -> g (x : t)) [] same as reverse" $
      quickCheck prop_cfold'_rev
