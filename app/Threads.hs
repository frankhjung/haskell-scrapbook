module Main (main) where

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.STM.TVar (modifyTVar', newTVar, readTVar,
                                              readTVarIO)
import           Control.Monad.STM           (atomically, check)
import           System.IO                   (BufferMode (LineBuffering),
                                              hSetBuffering, stdout)

-- | Example program using threads.
--
-- Threads example from
-- <https://typeclasses.com/phrasebook/threads TypeClasses Phrasebook>.
main :: IO ()
main = do

  -- make output more readable
  hSetBuffering stdout LineBuffering

  -- counter to be incremented
  tasksCompleted <- atomically (newTVar (0 :: Int))

  -- show thread and increment task completed flag
  -- "main" will end once each thread has incremented the task complete flag
  let task t = readTVarIO tasksCompleted
                >>= \c -> putStrLn (t ++ ": " ++ show c)
                >> atomically (modifyTVar' tasksCompleted (+ 1))

  -- run main and spawn threads (main is included in completed count)
  task "main"
  _ <- forkIO (task "fork1")
  _ <- forkIO (task "fork2")
  _ <- forkIO (task "fork3")

  -- check if threads have completed before terminating "main"
  atomically $ readTVar tasksCompleted >>= \x -> check (x > 3)

  -- terminate
  putStrLn "done"
