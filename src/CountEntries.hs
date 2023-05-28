{-|

Module      : CountEntries
Description : Count entries in a file path
Copyright   : © Frank Jung, 2020
License     : GPL-3

Function variations of counting directory entries from
<http://book.realworldhaskell.org/read/monad-transformers.html Chapter 18, Monad Transformers, Real World Haskell by Bryan O'Sullivan, Don Stewart, and John Goerzen>.
The original version in the book is `countEntriesTrad` which here is called
`countEntries0`.

The Monad Transformer version which in the book is `countEntries` is
`countEntries2` in this code.

The other versions `countEntries1` and `countEntries3` are my variations of
this code.

== GHCi Session

To test these functions in GHCi you need the following packages:

@
:m + Control.Monad
:m + System.Directory
:m + System.FilePath
@

=== Resources

This is a really good introduction to Monad Transformers,
<https://mmhaskell.com/monads/transformers Monday Morning Haskell: Monad Transformers>.

-}

module CountEntries ( countEntries0
                    , countEntries1
                    , countEntries2
                    , countEntries3
                    ) where

import           Control.Monad              (filterM, forM, forM_, when)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Writer (WriterT, execWriterT, tell)
import           System.Directory           (doesDirectoryExist, listDirectory)
import           System.FilePath            ((</>))

-- | Count entries in directories for given path.
--
-- This is the standard version from "Real World Haskell".
--
-- >>> p = "public"
--
-- >>> :t listDirectory p
-- listDirectory p :: IO [FilePath]
--
-- >>> countEntries0 p
-- [("public",25),("public/src",16)]
--
countEntries0 :: FilePath -> IO [(FilePath, Int)]
countEntries0 path = do
  contents <- listDirectory path                      -- contents of p
  rest <- forM contents $ \name -> do                 -- for each entry
            let newName = path </> name               -- full path name
            isDir <- doesDirectoryExist newName       -- is directory
            if isDir
              then countEntries0 newName              -- recurse
              else return []                          -- termination
  return $ (path, length contents) : concat rest      -- list and concat tuples

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

-- | My updated version to count entries in directories for given path.
--
-- This updated version of `countEntries1` uses `Control.Monad.Writer.WriterT`.
countEntries2 :: FilePath -> IO [(FilePath, Int)]
countEntries2 = execWriterT . countEntries2'
  where
    countEntries2' :: FilePath -> WriterT [(FilePath, Int)] IO ()
    countEntries2' path = do
      contents <- liftIO . listDirectory $ path               -- contents of path
      tell [(path, length contents)]                          -- show tuple
      forM_ contents $ \name -> do                            -- for each entry
        let newName = path </> name
        isDir <- liftIO . doesDirectoryExist $ newName        -- is directory
        when isDir $ countEntries2' newName                   -- recurse in sub-directories

-- | Count entries in directories for given path.
--
-- My version using `Control.Monad.Writer.WriterT`.
--
-- This function takes a `FilePath` argument and returns an IO action that,
-- when executed, returns a list of @(FilePath, Int)@ pairs representing
-- the number of entries in each directory.
--
-- The implementation of `countEntries3` uses the `execWriterT` function
-- from the `Control.Monad.Trans.Writer` module to extract the final value
-- of the writer monad and discard the log. The writer monad is used to
-- accumulate a list of @(FilePath, Int)@ pairs representing the number of
-- entries in each directory.
--
-- The `countEntries3` function is defined in terms of a helper function
-- `countEntries3'` that uses WriterT to accumulate the log of @(FilePath,
-- Int)@ pairs. The `countEntries3'` function first uses `listDirectory` to
-- get the contents of the directory specified by the `FilePath` argument.
-- It then uses `tell` to add a tuple of the directory path and the number
-- of entries in the directory to the log. Next, it uses `filterM` and
-- `doesDirectoryExist` to get a list of sub-directories in the directory.
-- Finally, it uses `mapM_` to recurse into each sub-directory and
-- accumulate the log.
--
-- The `countEntries3` function is defined using function composition,
-- where `countEntries3'` is composed with `execWriterT` using the @.@
-- operator. This creates a new function that first applies
-- `countEntries3'` to its input, and then applies `execWriterT` to the
-- output of `countEntries3'`.
--
countEntries3 :: FilePath -> IO [(FilePath, Int)]
countEntries3 = execWriterT . countEntries3'
  where
    countEntries3' :: FilePath -> WriterT [(FilePath, Int)] IO ()
    countEntries3' p =
      liftIO (listDirectory p)                                -- contents of path
      >>= \ps -> tell [(p, length ps)]                        -- show tuple
      >> liftIO (filterM (\n -> doesDirectoryExist (p </> n)) ps) -- sub-directories
      >>= \pss -> mapM_ (\n -> countEntries3' (p </> n)) pss  -- recurse into sub-directories
