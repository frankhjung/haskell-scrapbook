module CountEntriesSpec (spec) where

import           Control.Monad.Trans.Writer (execWriterT)
import           CountEntries               (countEntries1, countEntries2,
                                             countEntriesS, countEntriesU)
import           System.Directory           (makeAbsolute)
import           Test.Hspec                 (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "count directory entries" $ do
    let docs = 1              -- Number of files in @doc/@ directory
    it ("countEntries should return " ++ show docs) $ do
      p <- makeAbsolute "doc"
      ces1 <- countEntries1 p
      ces2 <- (execWriterT . countEntries2) p
      cesS <- countEntriesS p
      cesU <- (execWriterT . countEntriesU) p
      (snd . head) ces1 `shouldBe` docs
      (snd . head) ces2 `shouldBe` docs
      (snd . head) cesS `shouldBe` docs
      (snd . head) cesU `shouldBe` docs
