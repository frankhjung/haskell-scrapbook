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

Control.Monad FilterM
System.Directory doesDirectoryExist

λ> (getDirectoryContents "public") >>= filterM doesDirectoryExist
["..","."]

λ> countEntriesTrad "public"
[("public",25),("public/src",16)]

λ> p = "public"

λ> (getDirectoryContents p) >>= filterM (\n -> doesDirectoryExist (p </> n))
["..","src","."]

λ> listDirectory p >>= filterM (\n -> doesDirectoryExist (p </> n))
["src"]

λ> listDirectory p
["BinarySearch.html","SubSequence.html","While.html","meta.json","index.html","quick-jump.css","CFold.html","QSort.html","hslogo-16.png","CountEntries.html","WordCount.html","synopsis.png","minus.gif","haddock-bundle.min.js","Yahtzee.html","Mod35.html","MyLast.html","ReadFile.html","ZipExample.html","plus.gif","ocean.css","src","Threads.html","Cps.html","doc-index.html"]

λ> ps <- listDirectory p

λ> length ps
25

λ> listDirectory p >>= return . length
25

λ> listDirectory ("public" </> "src")
["BinarySearch.html","style.css","SubSequence.html","While.html","highlight.js","CFold.html","QSort.html","CountEntries.html","WordCount.html","Yahtzee.html","Mod35.html","MyLast.html","ReadFile.html","ZipExample.html","Threads.html","Cps.html"]

λ> listDirectory ("public" </> "src") >>= return . length
16

λ> ps >>= mapM_ print
"src"

λ> ps >>= mapM_ (listDirectory . (</>) p)

λ> listDirectory (p </> "src") >>= return . length
16

λ> listDirectory (p) >>= return . length
25

-}

{-# LANGUAGE TupleSections #-}

module CountEntries (main, countEntries, countEntriesTrad) where

import           Control.Monad    (filterM, forM, mapM_)
import           Data.Bool        (bool)
import           System.Directory (doesDirectoryExist, getCurrentDirectory,
                                   listDirectory)
import           System.FilePath  ((</>))

-- | Count entries for a list of paths.
countEntries :: FilePath -> IO [(FilePath, Int)]
countEntries p = do
  -- list subdirectories of p
  ps <- listDirectory p >>= filterM (\n -> doesDirectoryExist (p </> n))
  bool (mapM countEntry (p:ps)) (return []) (null p)
  where
    -- count of entries in a directory
    countEntry :: FilePath -> IO (FilePath, Int)
    countEntry a = (a,) . length <$> listDirectory a

-- | Count entries in directories for given path.
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
main = getCurrentDirectory >>= countEntries >>= mapM_ print

