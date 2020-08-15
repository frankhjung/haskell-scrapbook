module Main (main) where

import           BinarySearch               (bsearch)
import           CFold                      (cfold, cfold')
import           CountEntries               (countEntries1, countEntries2,
                                             countEntriesS, countEntriesU)
import           Cps                        (pythagorasCont)
import           Mod35                      (mod35)
import           MyLast                     (myLast, myRev1, myRev2,
                                             penultimate)
import           QSort                      (qsort)
import           RepMax                     (doRepMax, foldMax, traverseMax,
                                             traverseMax')
import           SplitList                  (splitMiddle)
import           State                      (Stack, empty, evalState, execState,
                                             tasks)
import           SubSeqs                    (subSeqs1, subSeqs2, subSeqs3,
                                             subSeqs4)
import           Yahtzee                    (DiceChoice (..), DiceVals,
                                             allRolls)

import           Control.Monad.Trans.Cont   (runCont)
import           Control.Monad.Trans.Writer
import           System.Directory           (makeAbsolute)
import           Test.Hspec                 (context, describe, hspec, it,
                                             shouldBe)

-- | Test call to 'Cps' function 'pythagorasCont'.
test_cps :: [Int] -> Maybe Int
test_cps (a:b:_) = runCont (pythagorasCont a b) Just
test_cps []      = Nothing
test_cps _       = Nothing

-- | Number of files in @app/@ directory
apps :: Int
apps = 5

-- | Test all modules
main :: IO ()
main = hspec $ do

  describe "binary search" $
    context "bsearch" $
      it "expect [Just 1, Just 2, Nothing, Nothing]" $ do
        let xs = [1, 2] :: [Int]
            ys = [3, 4] :: [Int]
            rs = [Just 1, Just 2, Nothing, Nothing]
        map (bsearch xs) (xs <> ys) `shouldBe` rs

  describe "continuation passing style - fold" $ do
    context "cfold (+) 0 [1..10] == sum [1..10]" $
      it "expect 55" $ do
        let xs = [1..10] :: [Int]
        cfold (+) 0 xs `shouldBe` sum xs
    context "cfold (:) [] [1..3] == [1,2,3]" $
      it "expect [1,2,3]" $ do
        let xs = [1,2,3] :: [Int]
        cfold (:) [] xs `shouldBe` xs
    context "cfold\' (\\x t g -> x : g t) [] ([1..10] :: [Int]) == [1..10]" $
      it "expect [1,2,3,4,5,6,7,8,9,10]" $ do
        let xs = [1..10] :: [Int]
        cfold' (\x t g -> x : g t) [] xs `shouldBe` xs
    context "cfold\' (\\x t g -> g (x : t)) [] ([1..10] :: [Int]) == [10..1]" $
      it "expect [10,9,8,7,6,5,4,3,2,1]" $ do
        let xs = [1..10] :: [Int]
        cfold' (\x t g -> g (x : t)) [] xs `shouldBe` reverse xs

  describe "continuation passing style - cps" $
    context "test_cps [3,4] == \"25\"" $
      it "expect 25" $ do
        let xs = [3,4] :: [Int]
        test_cps xs `shouldBe` Just 25

  describe "count directory entries" $ do
    -- countEntriesS p >>= mapM_ print . take 2
    context ("countEntriesS == " ++ show apps) $
      it ("expect " ++ show apps) $ do
        p <- makeAbsolute "app"
        ces <- countEntriesS p
        (snd . head) ces `shouldBe` apps
    -- countEntries1 p >>= mapM_ print . take 2
    context ("countEntries1 == " ++ show apps) $
      it ("expect " ++ show apps) $ do
        p <- makeAbsolute "app"
        ces <- countEntries1 p
        (snd . head) ces `shouldBe` apps
    -- (execWriterT . countEntriesU) p >>= mapM_ print . take 2
    context ("countEntriesU == " ++ show apps) $
      it ("expect " ++ show apps) $ do
        p <- makeAbsolute "app"
        ces <- (execWriterT . countEntriesU) p
        (snd . head) ces `shouldBe` apps
    -- (execWriterT . countEntries2) p >>= mapM_ print . take 2
    context ("countEntries2 == " ++ show apps) $
      it ("expect " ++ show apps) $ do
        p <- makeAbsolute "app"
        ces <- (execWriterT . countEntries2) p
        (snd . head) ces `shouldBe` apps

  describe "modulus 3 or 5 test" $ do
    context "mod35 3 = True" $
      it "expect True" $
      mod35 3 `shouldBe` True
    context "mod35 5 = True" $
      it "expect True" $
      mod35 5 `shouldBe` True
    context "mod35 15 = True" $
      it "expect True" $
      mod35 15 `shouldBe` True
    context "mod35 8 = False" $
      it "expect False" $
      mod35 8 `shouldBe` False

  describe "my last and penultimate" $ do
    context "myLast [1..5] = 5" $
      it "expect 5" $ do
        let xs = [1..5] :: [Int]
        last xs `shouldBe` myLast xs
    context "myLast [4] = 4" $
      it "expect 4" $ do
        let xs = [4] :: [Int]
        last xs `shouldBe` myLast xs
    context "penultimate empty list" $
      it "penultimate [] = Nothing" $ do
        let xs = [1] :: [Int]
        penultimate xs `shouldBe` Nothing
    context "penultimate simple list" $
      it "penultimate [1,2] = 1" $ do
        let xs = [1,2] :: [Int]
        penultimate xs `shouldBe` Just 1
    context "myRev1" $
      it "head . myRev1 = last" $ do
        let xs = [1..5] :: [Int]
        (head . myRev1) xs `shouldBe` last xs
    context "myRev2" $
      it "myRev1 = myRev2" $ do
        let xs = [1..5] :: [Int]
        myRev1 xs `shouldBe` myRev2 xs

    describe "qsort - naive / inefficient version" $
      context "sort unique elements" $
        it "expect [1,1,2,3,4,5]" $
          qsort ([1,3,5,1,4,2] :: [Int]) `shouldBe` ([1,1,2,3,4,5] :: [Int])

    describe "repeat maximum element from list" $ do
      context "doRepMax = foldMax" $
        it "expect equals" $ do
          let xs = [-2,-3,-1,-4,-5] :: [Int]
          doRepMax xs `shouldBe` foldMax xs
      context "foldMax = traverseMax" $
        it "expect equals" $ do
          let xs = [-2,-3,-1,-4,-5] :: [Int]
          foldMax xs `shouldBe` traverseMax xs
      context "traverseMax = traverseMax'" $
        it "expect equals" $ do
          let xs = [-2,-3,-1,-4,-5] :: [Int]
          traverseMax xs `shouldBe` traverseMax' xs

    describe "state using a stack" $ do
      context "evalState = 5" $
        it "expect equals" $
          evalState tasks empty `shouldBe` 5
      context "execState = [5,1]" $
        it "expect equals" $
          execState tasks empty `shouldBe` ([5,1] :: Stack)

    describe "generate sub-sequences" $ do
      context "subSeq1" $
        it "expect equal" $
          subSeqs1 "abc" `shouldBe` ["a","ab","abc","ac","b","bc","c"]
      context "subSeq2" $
        it "expect equal" $
          subSeqs2 "abc" `shouldBe` ["a","ab","abc","ac","b","bc","c"]
      context "subSeq3" $
        it "expect equal" $
          subSeqs3 "abc" `shouldBe` ["abc","ab","ac","a","bc","b","c",""]
      context "subSeq4" $
        it "expect equal" $
          subSeqs4 "abc" `shouldBe` ["abc","ab","ac","a","bc","b","c",""]

    describe "use zipWith to split a list in half" $
      context "splitMiddle" $
        it "expect equal" $
          splitMiddle "helloworld" `shouldBe` ("hello", "world")

    describe "test yahtzee from given start point" $
      context "example game" $
        it "expect equal" $ do
          let diceVals = [Reroll, Keep 4, Keep 4, Reroll, Reroll]
          last (allRolls diceVals) `shouldBe` ([6, 4, 4, 6, 6] :: DiceVals)

