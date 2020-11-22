{-# LANGUAGE ScopedTypeVariables #-}

module SubSeqsSpec (spec) where

import           SubSeqs                   (subSeqs1, subSeqs2, subSeqs3,
                                            subSeqs4)

import           Test.Hspec                (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck     (modifyMaxSize, prop)
import           Test.QuickCheck.Modifiers (NonEmptyList (NonEmpty))

spec :: Spec
spec =
  describe "generate sub-sequences" $ do
    it "subSeqs1 \"abc\" is [\"a\",\"ab\",\"abc\",\"ac\",\"b\",\"bc\",\"c\"]" $
      subSeqs1 "abc" `shouldBe` ["a","ab","abc","ac","b","bc","c"]
    it "subSeqs2 \"abc\" is [\"a\",\"ab\",\"abc\",\"ac\",\"b\",\"bc\",\"c\"]" $
      subSeqs2 "abc" `shouldBe` ["a","ab","abc","ac","b","bc","c"]
    it "subSeqs3" $
      subSeqs3 "abc" `shouldBe` ["abc","ab","ac","a","bc","b","c",""]
    it "subSeqs4" $
      subSeqs4 "abc" `shouldBe` ["abc","ab","ac","a","bc","b","c",""]
    modifyMaxSize (const 10) $
      prop "quickcheck subSeqs1 same as subSeqs2" $
        \(NonEmpty (xs :: String)) -> subSeqs1 xs == subSeqs2 xs
    modifyMaxSize (const 10) $
      prop "quickcheck subSeqs3 same as subSeqs4" $
        \(xs :: String) -> subSeqs3 xs == subSeqs4 xs
