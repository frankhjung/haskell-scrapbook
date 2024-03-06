#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

{-

Read contents of a file using
<https://hackage.haskell.org/package/base/docs/System-IO.html#v:withFile withFile>
function to process lines.

Read a file example, based off example in Chapter 2 of
<https://books.google.com.au/books/about/Haskell_Design_Patterns.html?id=Q_KoCwAAQBAJ&redir_esc=y Haskell Design Patterns by Ryan Lemmer>

TODO

- replace Strings with Text
-replace putStrLn with fmt

-}

module Main (main) where

import           Control.Exception  (IOException, catch, throwIO)
import           Control.Monad      ((<=<))
import           System.Environment (getArgs)

-- | Manage IO errors.
withErrorHandling :: IO () -> IO ()
withErrorHandling ioAction = catch ioAction handler
  where
    handler :: IOException -> IO ()
    handler = print

-- | Handle arguments
handleArgs :: IO (Either String FilePath)
handleArgs = getArgs >>= \case
  [filename] -> return $ Right @FilePath filename
  []         -> return $ Left "missing file name. Usage: readfile <file_name>"
  _          -> return $ Left "multiple arguments not supported"

-- | Process either error or file name.
eitherToErr :: Show a => Either a b -> IO b
eitherToErr = either (throwIO . userError . show) return

-- | Read contents of a file and print to STDOUT.
processFile :: FilePath -> IO ()
processFile = putStrLn <=< readFile

-- | Read a file name from the command line and print its contents to STDOUT.
main :: IO ()
main = withErrorHandling $ handleArgs >>= eitherToErr >>= processFile
