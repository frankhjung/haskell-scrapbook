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
            , usage
            , wordsFile
            ) where

import           Control.Arrow      (Kleisli (..), arr, runKleisli, (>>>))
import           Data.Maybe         (listToMaybe)
import           Fmt                (fmtLn, (+|), (|+))
import           System.Environment (getArgs)

-- | Show usage message.
usage :: String -> IO ()
usage = putStr . unlines . (: ["Usage: wordcountarrow <file path>"])

-- | Count words from a file path.
wordsFile :: FilePath -> IO ()
wordsFile file = runKleisli go file
  where
    -- | Count words from a file path.
    go :: Kleisli IO FilePath ()
    go = Kleisli readFile >>> arr words >>> arr length >>> Kleisli printCount
    -- | Print count and file path.
    printCount :: Int -> IO ()
    printCount count = fmtLn $ "" +| count |+ " " +| file |+ ""

-- | Count words in file.
--
-- >>> stack exec wordcountarrow -- Setup.hs
-- 5, Setup.hs
main :: IO ()
main = maybe (usage "Error: Missing file path") wordsFile . listToMaybe =<< getArgs
