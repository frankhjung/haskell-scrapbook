#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-

Read contents of a file using
<https://hackage.haskell.org/package/base/docs/System-IO.html#v:withFile withFile>
function to process lines.

Read a file example, based off example in Chapter 2 of
<https://books.google.com.au/books/about/Haskell_Design_Patterns.html?id=Q_KoCwAAQBAJ&redir_esc=y Haskell Design Patterns by Ryan Lemmer>

-}

module Main (main) where

import           Control.Exception  (catch, throwIO)
import           Control.Monad      ((<=<))
import qualified Data.Text.IO       as TIO
import           System.Environment (getArgs)

-- | Manage IO errors.
withErrorHandling :: IO () -> IO ()
withErrorHandling ioAction = catch ioAction handler
  where
    handler :: IOError -> IO ()
    handler = print

-- | Handle arguments
handleArgs :: IO (Either String FilePath)
handleArgs = getArgs >>= \case
  [filename] -> return $ Right @FilePath filename
  []         -> return $ Left "missing file name. Usage: readfile <file_name>"
  _          -> return $ Left "multiple arguments not supported"

-- | Process either error or file name.
eitherToError :: Show a => Either a b -> IO b
eitherToError = either (throwIO . userError . show) return

-- | Read contents of a file and print to STDOUT.
processFile :: FilePath -> IO ()
processFile = TIO.putStrLn <=< TIO.readFile

-- | Read a file name from the command line and print its contents to STDOUT.
main :: IO ()
main = withErrorHandling $ handleArgs >>= eitherToError >>= processFile
