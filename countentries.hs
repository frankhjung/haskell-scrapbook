#!/usr/bin/env runhaskell

{-|

Module      : CountEntries
Description : Count entries in a file path.
Copyright   : © Frank Jung, 2020
License     : GPL-3

From <http://book.realworldhaskell.org/read/monad-transformers.html Chapter 18, Monad Transformers, Real World Haskell by Bryan O'Sullivan, Don Stewart, and John Goerzen>.

This version `countEntriesTrad` uses traditional functional techniques
to recurse into a directory tree and returns a list of the number of
entries it finds at each level of the tree.

-}

module CountEntries (main, countEntriesTrad) where

import           Control.Monad    (forM, mapM_)
import           System.Directory (doesDirectoryExist, getCurrentDirectory,
                                   listDirectory)
import           System.FilePath  ((</>))

-- | Count entries in file path.
countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
              then countEntriesTrad newName
              else return []
  return $ (path, length contents) : concat rest

-- | Show count of entries from current path.
main :: IO ()
main = getCurrentDirectory >>= countEntriesTrad >>= mapM_ print
