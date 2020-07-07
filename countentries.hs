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

@
:m + Control.Monad
:m + System.Directory
:m + System.FilePath
@

-}

module CountEntries (main, countEntries1, countEntries2, countEntriesS, countEntriesU) where

import           Control.Monad        (filterM, forM, forM_, mapM_, when)
import           Control.Monad.Trans  (liftIO)
import           Control.Monad.Writer (WriterT, execWriterT, tell)
import           System.Directory     (doesDirectoryExist, getCurrentDirectory,
                                       listDirectory)
import           System.FilePath      ((</>))

-- | Count entries in directories for given path.
-- Standard version as documented in Real World Haskell.
--
-- >>> p = "public"
--
-- >>> :t listDirectory p
-- listDirectory p :: IO [FilePath]
--
-- >>> countEntriesS p
-- [("public",25),("public/src",16)]
--
countEntriesS :: FilePath -> IO [(FilePath, Int)]
countEntriesS path = do
  contents <- listDirectory path                              -- contents of p
  rest <- forM contents $ \name -> do                         -- for each entry
            let newName = path </> name                       -- full path name
            isDir <- doesDirectoryExist newName               -- is directory
            if isDir
              then countEntriesS newName                      -- recurse
              else return []                                  -- termination
  return $ (path, length contents) : concat rest              -- list and concat tuples

-- | Count entries for a list of paths. (My version.)
--
-- == Example
--
-- What the function returns is a list of tuples of directory and count of
-- entries in that directory:
--
-- >>> countEntries1 "public"
-- [("public",25),("public/src",16)]
--
-- === Explanation
--
-- The function composes a number of different system calls. But the
-- process is simple but clunky using traditional methods. The process will
-- be much simplified once when Monad Transformers are used in
-- `countEntries2`.
--
-- >>> p = "public"
--
-- Get contents of path ... and filter to report directories only:
--
-- >>> (getDirectoryContents p) >>= filterM (\n -> doesDirectoryExist (p </> n))
-- ["..","src","."]
--
-- List directory ignores current and parent directories:
--
-- >>> listDirectory p >>= filterM (\n -> doesDirectoryExist (p </> n))
-- ["src"]
--
-- Some ways to count number of entries in the path:
--
-- >>> ps <- listDirectory p
--
-- >>> length ps
-- 25
--
-- Same as:
--
-- >>> listDirectory p >>= return . length
-- 25
--
-- Which is equivalent to:
--
-- >>> liftM length (listDirectory p)
-- 25
--
-- Recurse into subdirectories:
--
-- >>> listDirectory p >>= filterM (\n -> doesDirectoryExist (p </> n)) >>= mapM_ print
-- "src"
--
countEntries1 :: FilePath -> IO [(FilePath, Int)]
countEntries1 p =
  if not (null p)
    then
      listDirectory p                                         -- contents of path
      >>= \ps -> filterM (\n -> doesDirectoryExist (p </> n)) ps -- sub-directories
      >>= mapM (\n -> countEntries1 (p </> n))                -- recurse
      >>= (\ces -> return $ (p, length ps) : ces) . concat    -- concat then list tuples
    else return []                                            -- termination

-- | Count entries in directories for given path.
-- Updated version using `Control.Monad.Writer.WriterT`.
countEntriesU :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntriesU path = do
  contents <- liftIO . listDirectory $ path                   -- contents of path
  tell [(path, length contents)]                              -- show tuple
  forM_ contents $ \name -> do                                -- for each entry
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName            -- is directory
    when isDir $ countEntriesU newName                        -- recurse

-- | Count entries in directories for given path.
-- My version using `Control.Monad.Writer.WriterT`.
countEntries2 :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries2 p =
  liftIO (listDirectory p)                                    -- contents of path
  >>= \ps -> tell [(p, length ps)]                            -- show tuple
  >> liftIO (filterM (\n -> doesDirectoryExist (p </> n)) ps) -- sub-directories
  >>= \pss -> mapM_ (\n -> countEntries2 (p </> n)) pss       -- recurse

-- | Show count of entries from current path.
main :: IO ()
main = do
  p <- getCurrentDirectory
  countEntriesS p >>= mapM_ print . take 2
  countEntries1 p >>= mapM_ print . take 2
  (execWriterT . countEntriesU) p >>= mapM_ print . take 2
  (execWriterT . countEntries2) p >>= mapM_ print . take 2
