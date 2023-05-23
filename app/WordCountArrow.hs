{-# LANGUAGE OverloadedStrings #-}

{-|

Count words in a file using Arrows.
See also `WordCount`.

>>> wc -w wordcount.hs
5 Setup.hs

>>> cabal exec wordcount -- Setup.hs
5, Setup.hs

>>> cabal exec wordcountarrow -- Setup.hs
5, Setup.hs

-}

module Main ( main
            , wordsFile
            ) where

import           Control.Arrow      (Kleisli (..), arr, runKleisli, (>>>))
import           Data.Maybe         (listToMaybe)
import           Fmt                (fmtLn, (+|), (|+))
import           System.Environment (getArgs)

-- | Show usage message.
usage :: String -> IO ()
usage msg = putStr (unlines [msg, "Usage: wordcountarrow <file path>"])

-- | Count words from a file path.
wordsFile :: FilePath -> IO ()
wordsFile file = runKleisli go file
  where
    go :: Kleisli IO FilePath ()
    go = Kleisli readFile >>> arr words >>> arr length >>> Kleisli printCount
    printCount :: Int -> IO ()
    printCount count = fmtLn $ "" +| count |+ ", " +| file |+ ""

-- | Count words in file.
--
-- >>> cabal exec wordcountarrow -- Setup.hs
-- 5
main :: IO ()
main = do
  args <- getArgs
  let fname = listToMaybe args :: Maybe FilePath
  maybe (usage "Missing file path") wordsFile fname
