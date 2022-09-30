#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

{-

Number lines of a file.

Similar to <https://man7.org/linux/man-pages/man1/nl.1.html nl(1)>.

>>> nl Setup.hs
  1  import Distribution.Simple
  2  main = defaultMain

>>> cabal exec numberlines -- Setup.hs
1  import Distribution.Simple
2  main = defaultMain

-}

module Main ( main
            , parseArgs
            , usage
            , numberlines
            , countNonBlankLines
            ) where

import           Data.Char          (isSpace)
import           Fmt                (padLeftF, (+|), (|+))
import           System.Environment (getArgs)

-- | Line number is a postive integer.
type LineNumber = Int

-- | Line is the file record to number.
type Line = String

-- | A numbered line.
data NumberedLine = Blank | Numbered LineNumber Line

-- | Custom show instance of NumberedLine.
-- 1. Padded line number + tab + non-bank line
-- 1. Empty lines are printed without a line number
instance Show NumberedLine where
  show Blank           = ""
  show (Numbered n ls) = "" +| padLeftF 5 ' ' n |+ "\t" +| ls |+ ""

-- | Parse command line arguments.
parseArgs :: [String] -> Maybe FilePath
parseArgs [file] = Just file
parseArgs _      = Nothing

-- | Show usage message.
usage :: String -> IO ()
usage err = putStrLn (unlines [err, "Usage: numberlines <file path>"])

-- | Count words from a file path.
numberlines :: FilePath -> IO ()
numberlines file = do
  content <- lines <$> readFile file
  let counted = countNonBlankLines content
  mapM_ print counted

-- | Count non-blank lines.
countNonBlankLines :: [Line] -> [NumberedLine]
countNonBlankLines ls =
  let go :: LineNumber -> [Line] -> [NumberedLine]
      go _ [] = []
      go n (x:xs)
        | all isSpace x = Blank : go n xs
        | otherwise = Numbered n x : go (succ n) xs
  in go 1 ls

-- | Number lines of a file.
--
-- >>> cabal exec numberlines -- Setup.hs
-- 1  import Distribution.Simple
-- 2  main = defaultMain
main :: IO ()
main = do
  args <- getArgs
  let mfile = parseArgs args
  maybe (usage "Missing file path") numberlines mfile
