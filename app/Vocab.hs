{-# LANGUAGE OverloadedStrings #-}

{-|

 Extract vocabulary of a file and list number of distinct words.

 Adapted from <https://www.manning.com/books/haskell-in-depth Haskell in Depth
 by Vitaly Bragilevsky>

 -}

module Main ( main
            , vocabFile
            , vocabText
            ) where

import           Data.Char          (isLetter)
import           Data.List          (group, sort)
import           Data.Maybe         (listToMaybe)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           System.Environment (getArgs)

-- | Show usage message.
usage :: String -> IO ()
usage msg = putStr (unlines [msg, "Usage: wordcount <file path>"])

-- | Extract, count and print distinct words from a file path.
vocabFile :: FilePath -> IO ()
vocabFile file = do
  ftext <- TIO.readFile file
  let ws = vocabText ftext
  TIO.putStrLn $ T.unwords ws
  print $ length ws

-- | Extract distinct words from text.
vocabText :: T.Text -> [T.Text]
vocabText = map head . group . sort
              . map T.toCaseFold . filter (not . T.null)
              . map (T.dropAround $ not . isLetter) . T.words

main :: IO ()
main = do
  args <- getArgs
  let fname = listToMaybe args :: Maybe FilePath
  maybe (usage "Missing file path") vocabFile fname
