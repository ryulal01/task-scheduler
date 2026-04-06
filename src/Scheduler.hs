module Scheduler where

import Types

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Time.Clock

import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Max as PQ

-- aging: увеличиваем приоритет со временем
effectivePriority :: UTCTime -> Task -> Int
effectivePriority now t =
  let age :: Int
      age = floor (diffUTCTime now (tCreated t))
  in tPriority t + age `div` 5

depsFinished :: Pool -> [TaskId] -> STM Bool
depsFinished pool deps = do
  tasks <- readTVar (poolTasks pool)
  and <$> mapM (check tasks) deps
  where
    check tasks tid =
      case M.lookup tid tasks of
        Nothing -> pure True
        Just t  -> (== Finished) <$> readTVar (tStatus t)

enqueueTask :: Pool -> Task -> IO ()
enqueueTask pool task = do
  now <- getCurrentTime
  atomically $
    modifyTVar' (poolReady pool)
      (PQ.insert (effectivePriority now task) (tId task))

startTask :: Pool -> Task -> IO ()
startTask pool task = do
  atomically $ writeTVar (tStatus task) Running

  tid <- forkIO $ do
    res <- try (tAction task)
    atomically $ writeTVar (tResult task) (Just res)
    finishTask pool task

  atomically $ writeTVar (tThread task) (Just tid)

finishTask :: Pool -> Task -> IO ()
finishTask pool task = do
  atomically $ do
    writeTVar (tStatus task) Finished
    modifyTVar' (poolFreeCores pool) (+ tCores task)

  deps <- atomically $ readTVar (tDependents task)

  forM_ deps $ \tid -> do
    mt <- atomically $ do
      tasks <- readTVar (poolTasks pool)
      pure (M.lookup tid tasks)

    case mt of
      Nothing -> pure ()
      Just t -> do
        ready <- atomically $ depsFinished pool (tDeps t)
        when ready $ enqueueTask pool t

scheduler :: Pool -> IO ()
scheduler pool = forever $ do
  mtask <- atomically $ do
    q <- readTVar (poolReady pool)
    free <- readTVar (poolFreeCores pool)

    case PQ.getMax q of
      Nothing -> retry
      Just (_, tid) -> do
        tasks <- readTVar (poolTasks pool)

        case M.lookup tid tasks of
          Nothing -> do
            writeTVar (poolReady pool) (PQ.deleteMax q)
            pure Nothing

          Just t -> do
            st <- readTVar (tStatus t)
            if st == Cancelled
              then do
                writeTVar (poolReady pool) (PQ.deleteMax q)
                pure Nothing
              else if tCores t <= free
                then do
                  writeTVar (poolReady pool) (PQ.deleteMax q)
                  writeTVar (poolFreeCores pool) (free - tCores t)
                  pure (Just t)
                else retry

  case mtask of
    Nothing -> pure ()
    Just t  -> startTask pool t
