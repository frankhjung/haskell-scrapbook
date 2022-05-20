{-# LANGUAGE ScopedTypeVariables #-}

module ZipFoldSpec (spec) where

import qualified ZipFold               (zip, zip')

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)

-- | Prelude.zip same as ZipFold.zip for lists
-- prop_zipfold :: NonEmptyList [Int] -> NonEmptyList String -> Property
-- prop_zipfold (NonEmpty xs) (NonEmpty ys) = ZipFold.zip xs ys === zip xs ys

spec :: Spec
spec = do

  describe "ZipFold.zip same as Prelude.zip" $
    prop "expect same" $
      \(xs :: [Int], ys :: String) -> ZipFold.zip xs ys == zip xs ys

  describe "ZipFold.zip' same as Prelude.zip" $
    prop "expect same" $
      \(is :: [Int], as :: String) -> ZipFold.zip' is as == zip is as
