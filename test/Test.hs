import           BinarySearch               (bsearch)
import           CFold                      (cfold, cfold')
import           CountEntries               (countEntries1, countEntries2,
                                             countEntriesS, countEntriesU)
import           Cps                        (pythagorasCont)
import           Mod35                      (mod35)
import           MyLast                     (myLast, myRev1, myRev2,
                                             penultimate)
import           Qsort                      (qsort)
import           RepMax                     (doRepMax, foldMax, traverseMax,
                                             traverseMax')
import           SplitList                  (splitMiddle)
import           Stack                      (Stack, empty, tasks)
import           State                      (evalState, execState)
import           SubSeqs                    (subSeqs1, subSeqs2, subSeqs3,
                                             subSeqs4)
import           Yahtzee                    (DiceChoice (..), DiceVals,
                                             allRolls)

import           Control.Monad.Trans.Cont   (runCont)
import           Control.Monad.Trans.Writer
import           Data.List                  (sort)
import           System.Directory           (makeAbsolute)
import           Test.Hspec                 (describe, hspec, it, shouldBe)
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

-- | Continuation Passing Style pythagoras
prop_pythagoras :: Int -> Int -> Bool
prop_pythagoras a b = runCont (pythagorasCont a b) Just == Just (a*a + b*b)

-- | Head of myRev1 is same as last
prop_myRev1 :: NonEmptyList [Int] -> Bool
prop_myRev1 (NonEmpty xs) = (head . myRev1) xs == last xs

-- | myRev1 is same as myRev2
prop_myRev1_myRev2 :: [Int] -> Bool
prop_myRev1_myRev2 xs = myRev1 xs == myRev2 xs

-- | Qsort same as 'Data.List.sort'
prop_qsort :: NonEmptyList [Int] -> Bool
prop_qsort (NonEmpty xs) = qsort xs == sort xs

-- | Qsort is idempotent
prop_qsort' :: NonEmptyList [Int] -> Property
prop_qsort' (NonEmpty xs) = qsort (qsort xs) === qsort xs

-- | 'doRepMax' is same as 'foldMax'
prop_doRepMax_foldMax :: [Int] -> Bool
prop_doRepMax_foldMax xs = doRepMax xs == foldMax xs

-- | 'foldMax' is same as 'traverseMax'
prop_foldMax_traverseMax :: [Int] -> Bool
prop_foldMax_traverseMax xs = foldMax xs == traverseMax xs

-- | 'traverseMax' is same as 'traverseMax\''
prop_traverseMax_traverseMax' :: [Int] -> Bool
prop_traverseMax_traverseMax' xs = traverseMax xs == traverseMax' xs

-- | 'traverseMax' is idempotent
prop_traverseMax :: [Int] -> Property
prop_traverseMax xs = traverseMax (traverseMax xs) === traverseMax xs

-- | 'subSeqs1' same as 'subSeqs2'
prop_subSeqs_1_2 :: NonEmptyList String -> Bool
prop_subSeqs_1_2 (NonEmpty xs) = subSeqs1 xs == subSeqs2 xs

-- | 'subSeqs3' same as 'subSeqs4'
prop_subSeqs_3_4 :: NonEmptyList String -> Bool
prop_subSeqs_3_4 (NonEmpty xs) = subSeqs3 xs == subSeqs4 xs

-- | Test all modules
main :: IO ()
main = hspec $ do

  describe "binary search" $
    it "find only 1 and 2" $ do
      let xs = [1, 2] :: [Int]
          ys = [3, 4] :: [Int]
      map (bsearch xs) (xs <> ys) `shouldBe` [Just 1, Just 2, Nothing, Nothing]

  describe "continuation passing style - fold" $ do
    it "cfold (+) 0 same as sum" $
      quickCheck prop_cfold_sum
    it "cfold (:) [] same as cons" $
      quickCheck prop_cfold_cons
    it "cfold\' (\\x t g -> x : g t) [] same as cons" $
      quickCheck prop_cfold'_cons
    it "cfold\' (\\x t g -> g (x : t)) [] same as reverse" $
      quickCheck prop_cfold'_rev

  describe "continuation passing style - cps" $ do
    it "pythagoras 3 4 is 25" $
      runCont (pythagorasCont 3 4) Just `shouldBe` Just 25
    it "pythagoras a b is a*a + b*b" $
      quickCheck prop_pythagoras

  describe "count directory entries" $ do
    let docs = 3              -- Number of files in @doc/@ directory
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

  describe "test if modulus 3 or 5" $ do
      it "mod35 3 is True" $ mod35 3 `shouldBe` True
      it "mod35 5 is True" $ mod35 5 `shouldBe` True
      it "mod35 15 is True" $ mod35 15 `shouldBe` True
      it "mod35 8 is False" $ mod35 8 `shouldBe` False

  describe "test last and penultimate of lists" $ do
    let xs = [1..5] :: [Int]
    it "myLast [1..5] is 5" $
      last xs `shouldBe` myLast xs
    it "myLast [4] is 4" $
      last ([4] :: [Int]) `shouldBe` myLast [4]
    it "penultimate [1] is Nothing" $
      penultimate ([1] :: [Int]) `shouldBe` Nothing
    it "penultimate [1,2] is 1" $
      penultimate ([1,2] :: [Int]) `shouldBe` Just 1
    it "head . myRev1 is last" $
      quickCheck prop_myRev1
    it "myRev1 is myRev2" $
      quickCheck prop_myRev1_myRev2

  describe "qsort - naive / inefficient version" $ do
    it "qsort example" $
      qsort ([1,3,5,1,4,2] :: [Int]) `shouldBe` ([1,1,2,3,4,5] :: [Int])
    it "qsort same as Data.List.sort" $
      quickCheck prop_qsort
    it "qsort is idempotent" $
      quickCheck prop_qsort'

  describe "replace list with maximum element" $ do
    let xs = [-2,-3,-1,-4,-5] :: [Int]
    it "doRepMax is same as foldMax" $
      doRepMax xs `shouldBe` foldMax xs
    it "foldMax is same as traverseMax" $
      foldMax xs `shouldBe` traverseMax xs
    it "traverseMax is same as traverseMax'" $
      traverseMax xs `shouldBe` traverseMax' xs
    it "quick check doRepMax is same as foldMax" $
      quickCheck prop_doRepMax_foldMax
    it "quick check foldMax is same as traverseMax" $
      quickCheck prop_foldMax_traverseMax
    it "quick check traverseMax is same as traverseMax'" $
      quickCheck prop_traverseMax_traverseMax'
    it "check check traverseMax is idempotent" $
      quickCheck prop_traverseMax

  describe "state using a stack" $ do
    it "evalState is 5" $
      evalState tasks empty `shouldBe` 5
    it "execState is [5,1]" $
      execState tasks empty `shouldBe` ([5,1] :: Stack)

  describe "generate sub-sequences" $ do
    it "subSeqs1" $
      subSeqs1 "abc" `shouldBe` ["a","ab","abc","ac","b","bc","c"]
    it "subSeqs2" $
      subSeqs2 "abc" `shouldBe` ["a","ab","abc","ac","b","bc","c"]
    it "subSeqs3" $
      subSeqs3 "abc" `shouldBe` ["abc","ab","ac","a","bc","b","c",""]
    it "subSeqs4" $
      subSeqs4 "abc" `shouldBe` ["abc","ab","ac","a","bc","b","c",""]
    it "subSeqs1 same as subSeqs2" $
      quickCheckWith stdArgs { maxSize = 10 } prop_subSeqs_1_2
    it "subSeqs3 same as subSeqs4" $
      quickCheckWith stdArgs { maxSize = 10 } prop_subSeqs_3_4

  describe "use zipWith to split a list in half" $
    it "expect equal" $
      splitMiddle "helloworld" `shouldBe` ("hello", "world")

  describe "test yahtzee from given start point" $
    it "example game" $ do
      let diceVals = [Reroll, Keep 4, Keep 4, Reroll, Reroll]
      last (allRolls diceVals) `shouldBe` ([6, 4, 4, 6, 6] :: DiceVals)
