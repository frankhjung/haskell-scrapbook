#!/usr/bin/env runhaskell

{-|

Module      : ReadFile
Description : Read a file.
Copyright   : © Frank Jung, 2020
License     : GPL-3

Read a file example, based off example in Chapter 2 of
<https://books.google.com.au/books/about/Haskell_Design_Patterns.html?id=Q_KoCwAAQBAJ&redir_esc=y Haskell Design Patterns by Ryan Lemmer>

-}

module ReadFile (main) where

import           Control.Monad ((>=>))
import           System.IO     (IOMode (ReadMode), hGetContents, withFile)

-- | Read contents of a file using
--  [withFile](https://hackage.haskell.org/package/base/docs/System-IO.html#v:withFile)
--  function to process lines.
--
-- The original version from 'Haskell Design Patterns' was:
--
-- @
-- main = do
--   withFile "inputfile.txt" ReadMode enumerateLines
--   where
--     enumerateLines h = lines' h >>= mapM_ putStrLn
--     lines' h' = hGetContents h' >>= return . lines
-- @
--
-- My version:
--
-- @ getLine >>= \\f -> withFile f ReadMode (hGetContents >=> mapM_ putStrLn . lines) @
--
--  - has parameterised the file to read
--  - does not use the [do notation](https://en.wikibooks.org/wiki/Haskell/do_notation)
--  - uses [Control.Monad](https://hackage.haskell.org/package/base/docs/Control-Monad.html)'s
--    left-to-right composition of Kleisli arrows
--    ( [>=>](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html#v:-62--61--62-) )
--    to pass the file handle
--
main :: IO ()
main = putStr "enter name of file to echo: "
  >> getLine
  >>= \f -> withFile f ReadMode (hGetContents >=> mapM_ putStrLn . lines)

