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

import           Control.Monad      ((>=>))
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
usage = putStrLn . unlines . (: ["Usage: numberlines <file path>"])

-- | Print & number non-blank lines in a file.
--
-- readFile :: (FilePath -> IO String)
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- lines :: (String -> [String])
-- countNonBlankLines :: ([String] -> [NumberedLine])
-- (.) :: (b -> c) -> (a -> b) -> a -> c
numberlines :: FilePath -> IO ()
numberlines = readFile >=> mapM_ print . countNonBlankLines . lines

-- | Count non-blank lines.
--
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- Numbered :: LineNumber -> Line -> NumberedLine
-- [1..] :: [LineNumber]
-- filter :: (a -> Bool) -> [a] -> [a]
-- all :: (a -> Bool) -> [a] -> Bool
-- isSpace :: Char -> Bool
countNonBlankLines :: [Line] -> [NumberedLine]
countNonBlankLines = zipWith Numbered [1..] . filter (not . all isSpace)

-- | Number lines of a given file.
--
-- getArgs :: IO [String]
-- parseArgs :: [String] -> Maybe FilePath
-- numberlines :: FilePath -> IO ()
-- maybe :: b -> (a -> b) -> Maybe a -> b
main :: IO ()
main = maybe (usage "Error: Missing file path") numberlines . parseArgs =<< getArgs
