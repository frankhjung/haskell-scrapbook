#!/usr/bin/env runhaskell

{-

Read contents of a file using
<https://hackage.haskell.org/package/base/docs/System-IO.html#v:withFile withFile>
function to process lines.

Read a file example, based off example in Chapter 2 of
<https://books.google.com.au/books/about/Haskell_Design_Patterns.html?id=Q_KoCwAAQBAJ&redir_esc=y Haskell Design Patterns by Ryan Lemmer>

The original version:

@
main = do
  withFile "Setup.hs" ReadMode enumerateLines
  where
    enumerateLines h = lines' h >>= mapM_ putStrLn
    lines' h' = hGetContents h' >>= return . lines
@

Version which reads file name from STDIN:

@
main = putStr "enter name of file to echo: "
  >> getLine
  >>= \f -> withFile f ReadMode (hGetContents >=> mapM_ putStrLn . lines)
@

Here is my version:

@
getLine >>= \\f -> withFile f ReadMode (hGetContents >=> mapM_ putStrLn . lines)
@

Where:

 * has parameterised the file to read

 * does not use the [do notation](https://en.wikibooks.org/wiki/Haskell/do_notation)

 * uses [Control.Monad](https://hackage.haskell.org/package/base/docs/Control-Monad.html)'s
   left-to-right composition of Kleisli arrows
   ( [>=>](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html#v:-62--61--62-) )
   to pass the file handle

-}

module Main (main) where

import           Control.Monad ((>=>))
import           System.IO     (IOMode (ReadMode), hGetContents, withFile)

main :: IO ()
main = withFile "Setup.hs" ReadMode (hGetContents >=> mapM_ putStrLn . lines)
