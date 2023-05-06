{-# LANGUAGE OverloadedStrings #-}

{-|

Count words in a file.

Similar to <https://linux.die.net/man/1/wc wc(1)>.

>>> wc -w wordcount.hs
5 Setup.hs

>>> cabal exec wordcount -- Setup.hs
5, Setup.hs

-}

module Main ( main
            , parseArgs
            , wordsFile
            ) where

import           Fmt                (fmtLn, (+|), (|+))
import           System.Environment (getArgs)

-- | Parse command line arguments.
-- This can be replaced by `listToMaybe` from `Data.Maybe`.
parseArgs :: [String] -> Maybe FilePath
parseArgs [file] = Just file
parseArgs _      = Nothing

-- | Show usage message.
usage :: String -> IO ()
usage msg = putStr (unlines [msg, "Usage: wordcount <file path>"])

-- | Count words from a file path.
wordsFile :: FilePath -> IO ()
wordsFile file = do
  count <- length . words <$> readFile file
  fmtLn $ "" +|count|+ ", " +|file|+ ""

-- | Count words in file.
--
-- >>> cabal exec wordcount -- Setup.hs
-- 5, Setup.hs
main :: IO ()
main = do
  args <- getArgs
  let fname = parseArgs args
  maybe (usage "Missing file path") wordsFile fname
