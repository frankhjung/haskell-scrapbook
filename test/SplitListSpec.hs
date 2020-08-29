module SplitListSpec (spec) where
import           SplitList       (splitMiddle)

import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck

-- | splitMiddle
prop_splitMiddle :: String -> Bool
prop_splitMiddle xs = splitMiddle (xs ++ xs) == (xs, xs)

spec :: Spec
spec =
  describe "use zipWith to split a list in half" $ do
    it "expect (hello, world)" $
      splitMiddle "helloworld" `shouldBe` ("hello", "world")
    it "quickcheck (xs, xs)" $
      quickCheck prop_splitMiddle
