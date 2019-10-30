#!/usr/bin/runhaskell

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.STM.TVar (modifyTVar', newTVar, readTVar)
import           Control.Monad.STM           (atomically, check)
import           System.IO                   (BufferMode (LineBuffering),
                                              hSetBuffering, stdout)

-- | Threads example from https://typeclasses.com/phrasebook/threads
main :: IO ()
main = do

  -- make output more readable
  hSetBuffering stdout LineBuffering

  -- counter to be incremented
  tasksCompleted <- atomically (newTVar 0)

  -- show thread and task completed counter
  let task t = mapM_ (\i -> putStrLn (t ++ ": " ++ show i)) [1..3]
                >> atomically (modifyTVar' tasksCompleted (+ 1))

  -- run main and spawn threads
  task "main"
  _ <- forkIO (task "forkA")
  _ <- forkIO (task "forkB")

  -- check if threads have completed before terminating "main"
  atomically $ readTVar tasksCompleted >>= \x -> check (x == 3)

  -- terminate
  putStrLn "done"
