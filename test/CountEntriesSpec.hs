module CountEntriesSpec (spec) where

import           CountEntries     (countEntries0, countEntries1, countEntries2,
                                   countEntries3)
import           System.Directory (makeAbsolute)
import           Test.Hspec       (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "count directory entries" $ do
  let docs = 1 -- Number of files in @doc/@ directory
  it "countEntries0 should return the correct number of entries" $ do
    p <- makeAbsolute "doc"
    ces <- countEntries0 p
    ces `shouldBe` [(p, docs)]

  it "countEntries1 should return the correct number of entries" $ do
    p <- makeAbsolute "doc"
    ces <- countEntries1 p
    ces `shouldBe` [(p, docs)]

  it "countEntries2 should return the correct number of entries" $ do
    p <- makeAbsolute "doc"
    ces <- countEntries2 p
    ces `shouldBe` [(p, docs)]

  it "countEntries3 should return the correct number of entries" $ do
    p <- makeAbsolute "doc"
    ces <- countEntries3 p
    ces `shouldBe` [(p, docs)]
