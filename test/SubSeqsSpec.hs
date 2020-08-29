module SubSeqsSpec (spec) where
import           SubSeqs         (subSeqs1, subSeqs2, subSeqs3, subSeqs4)

import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck

-- | subSeqs1 same as subSeqs2
prop_subSeqs_1_2 :: NonEmptyList String -> Bool
prop_subSeqs_1_2 (NonEmpty xs) = subSeqs1 xs == subSeqs2 xs

-- | subSeqs3 same as subSeqs4
prop_subSeqs_3_4 :: NonEmptyList String -> Bool
prop_subSeqs_3_4 (NonEmpty xs) = subSeqs3 xs == subSeqs4 xs

spec :: Spec
spec =
  describe "generate sub-sequences" $ do
    it "subSeqs1" $
      subSeqs1 "abc" `shouldBe` ["a","ab","abc","ac","b","bc","c"]
    it "subSeqs2" $
      subSeqs2 "abc" `shouldBe` ["a","ab","abc","ac","b","bc","c"]
    it "subSeqs3" $
      subSeqs3 "abc" `shouldBe` ["abc","ab","ac","a","bc","b","c",""]
    it "subSeqs4" $
      subSeqs4 "abc" `shouldBe` ["abc","ab","ac","a","bc","b","c",""]
    it "quickcheck subSeqs1 same as subSeqs2" $
      quickCheckWith stdArgs { maxSize = 10 } prop_subSeqs_1_2
    it "quickcheck subSeqs3 same as subSeqs4" $
      quickCheckWith stdArgs { maxSize = 10 } prop_subSeqs_3_4
