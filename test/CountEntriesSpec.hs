module CountEntriesSpec (spec) where

import           CountEntries     (countEntries0, countEntries1, countEntries2,
                                   countEntries3)
import           System.Directory (makeAbsolute)
import           Test.Hspec       (Spec, describe, it, shouldReturn)

spec :: Spec
spec = describe "count directory entries" $ do
  let docs = 1 -- Number of files in @doc/@ directory
  it "countEntries0 should return the correct number of entries" $ do
    p <- makeAbsolute "doc"
    countEntries0 p `shouldReturn` [(p, docs)]

  it "countEntries1 should return the correct number of entries" $ do
    p <- makeAbsolute "doc"
    countEntries1 p `shouldReturn` [(p, docs)]

  it "countEntries2 should return the correct number of entries" $ do
    p <- makeAbsolute "doc"
    countEntries2 p `shouldReturn` [(p, docs)]

  it "countEntries3 should return the correct number of entries" $ do
    p <- makeAbsolute "doc"
    countEntries3 p `shouldReturn` [(p, docs)]
