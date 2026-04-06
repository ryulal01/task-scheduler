module API where

import Types
import Scheduler
import Pool

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Time.Clock

import qualified Data.Map.Strict as M

submit
  :: MonadUnliftIO m
  => Int
  -> Int
  -> m ()
  -> [TaskId]
  -> Pool
  -> m TaskId
submit prio cores action deps pool =
  withRunInIO $ \run -> do

    tid <- atomically $ newId pool
    now <- getCurrentTime

    statusVar     <- newTVarIO Pending
    threadVar     <- newTVarIO Nothing
    dependentsVar <- newTVarIO []
    resultVar     <- newTVarIO Nothing

    let task = Task
          tid
          (run action)
          deps
          dependentsVar
          statusVar
          threadVar
          resultVar
          prio
          cores
          now

    atomically $ do
      modifyTVar' (poolTasks pool) (M.insert tid task)

      tasks <- readTVar (poolTasks pool)
      forM_ deps $ \d ->
        case M.lookup d tasks of
          Just dep -> modifyTVar' (tDependents dep) (tid:)
          Nothing  -> pure ()

    ready <- atomically $ depsFinished pool deps
    when ready $ enqueueTask pool task

    pure tid
