{-# LANGUAGE OverloadedStrings #-}

{-|

Count words in a file.

Similar to <https://linux.die.net/man/1/wc wc(1)>.

>>> wc -w Setup.hs
5 Setup.hs

>>> stack exec -- wordcount Setup.hs
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
usage = putStr . unlines . (: ["Usage: wordcount <file path>"])

-- | Count words from a file path.
wordsFile :: FilePath -> IO ()
wordsFile file = fmtLn . ("" +|) . (|+ " " +|file|+ "") . length . words =<< readFile file

-- | Count words in file.
--
-- >>> cabal exec wordcount -- Setup.hs
-- 5, Setup.hs
main :: IO ()
main = maybe (usage "Error: Missing file path") wordsFile . parseArgs =<< getArgs
