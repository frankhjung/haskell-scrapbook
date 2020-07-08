#!/usr/bin/env runhaskell

{-|

Module      : Threads
Description : Example program using threads.
Copyright   : © Frank Jung, 2019
License     : GPL-3

Threads example from <https://typeclasses.com/phrasebook/threads TypeClasses Phrasebook>.

-}

module Threads (main) where

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.STM.TVar (modifyTVar', newTVar, readTVar,
                                              readTVarIO)
import           Control.Monad.STM           (atomically, check)
import           System.IO                   (BufferMode (LineBuffering),
                                              hSetBuffering, stdout)

-- | Example program using threads.
main :: IO ()
main = do

  -- make output more readable
  hSetBuffering stdout LineBuffering

  -- counter to be incremented
  tasksCompleted <- atomically (newTVar 0)

  -- show thread and increment task completed flag
  -- "main" will end once each thread has incremented the task complete flag
  let task t = readTVarIO tasksCompleted
                >>= \c -> putStrLn (t ++ ": " ++ show c)
                >> atomically (modifyTVar' tasksCompleted (+ 1))

  -- run main and spawn threads (main is included in completed count)
  task "main"
  _ <- forkIO (task "forkA")
  _ <- forkIO (task "forkB")
  _ <- forkIO (task "forkC")

  -- check if threads have completed before terminating "main"
  atomically $ readTVar tasksCompleted >>= \x -> check (x > 3)

  -- terminate
  putStrLn "done"
