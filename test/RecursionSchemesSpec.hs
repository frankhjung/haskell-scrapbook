{-# LANGUAGE ScopedTypeVariables #-}
module RecursionSchemesSpec (spec) where

import           Numeric.Natural       (Natural)
import           RecursionSchemes      (Fix (..), ListF (..), buildListF,
                                        lengthListF, toList)

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (NonNegative (..))

ls :: Fix (ListF Int)
ls = Fix (ConsF 4 (Fix (ConsF 3 (Fix (ConsF 2 (Fix (ConsF 1 (Fix NilF))))))))

spec :: Spec
spec = do
  describe "catamorphism" $
    it "have length of 4" $
      lengthListF ls `shouldBe` 4
  describe "anamorphism" $ do
    it "build list has length of 4" $
      (lengthListF . buildListF) 4 `shouldBe` 4
    it "build list same as constant list" $
      toList (buildListF 4) `shouldBe` toList ls
  describe "quickcheck random list generation" $
      prop "quickcheck list length same as list build" $
        \(NonNegative (n :: Int)) -> (lengthListF . buildListF) n `shouldBe` n
