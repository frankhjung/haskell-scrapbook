#!/usr/bin/env runhaskell

{-|

Module      : CountEntries
Description : Count entries in a file path.
Copyright   : © Frank Jung, 2020
License     : GPL-3

From <http://book.realworldhaskell.org/read/monad-transformers.html Chapter 18, Monad Transformers, Real World Haskell by Bryan O'Sullivan, Don Stewart, and John Goerzen>.

There are two versions in this example. The first
version `countEntriesTrad` uses traditional functional techniques
to recurse into a directory tree and returns a list of the number of
entries it finds at each level of the tree.

I re-wrote this as `countEntries` to remove for loops and do-notation.

== GHCi Session

=== Packages

> Control.Monad
> System.Directory
> System.FilePath

=== Examples

>>> (getDirectoryContents "public") >>= filterM doesDirectoryExist
["..","."]

>>> countEntriesTrad "public"
[("public",25),("public/src",16)]

>>> p = "public"

>>> (getDirectoryContents p) >>= filterM (\n -> doesDirectoryExist (p </> n))
["..","src","."]

>>> listDirectory p
["BinarySearch.html","SubSequence.html","While.html","meta.json","index.html","quick-jump.css","CFold.html","QSort.html","hslogo-16.png","CountEntries.html","WordCount.html","synopsis.png","minus.gif","haddock-bundle.min.js","Yahtzee.html","Mod35.html","MyLast.html","ReadFile.html","ZipExample.html","plus.gif","ocean.css","src","Threads.html","Cps.html","doc-index.html"]

>>> listDirectory p >>= filterM (\n -> doesDirectoryExist (p </> n))
["src"]

>>> ps <- listDirectory p

>>> length ps
25

>>> listDirectory p >>= return . length
25

>>> liftM length (listDirectory p)
25

>>> ps >>= mapM_ print
"src"

>>> listDirectory ("public" </> "src")
["BinarySearch.html","style.css","SubSequence.html","While.html","highlight.js","CFold.html","QSort.html","CountEntries.html","WordCount.html","Yahtzee.html","Mod35.html","MyLast.html","ReadFile.html","ZipExample.html","Threads.html","Cps.html"]

>>> listDirectory (p </> "src") >>= return . length
16

>>> liftM length (listDirectory (p </> "src"))
16

-}

module CountEntries (main, countEntries, countEntriesTrad) where

import           Control.Monad    (filterM, forM, mapM_)
import           System.Directory (doesDirectoryExist, getCurrentDirectory,
                                   listDirectory)
import           System.FilePath  ((</>))

-- | Count entries for a list of paths.
countEntries :: FilePath -> IO [(FilePath, Int)]
countEntries p =
  if not (null p)
    then
      listDirectory p                                         -- contents of p
      >>= \ps -> filterM (\n -> doesDirectoryExist (p </> n)) ps -- sub-directories
      >>= mapM (\n -> countEntries (p </> n))                 -- recurse
      >>= (\ces -> return $ (p, length ps) : ces) . concat    -- concat results
    else return []                                            -- termination

-- | Count entries in directories for given path.
countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path                              -- contents of p
  rest <- forM contents $ \name -> do                         -- for each
            let newName = path </> name                       -- full path name
            isDir <- doesDirectoryExist newName               -- is directory
            if isDir
              then countEntriesTrad newName                   -- recurse
              else return []                                  -- termination
  return $ (path, length contents) : concat rest              -- concat results

-- | Show count of entries from current path.
main :: IO ()
main = getCurrentDirectory >>= countEntries >>= mapM_ print

