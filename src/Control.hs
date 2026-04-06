module Control where

import Types

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Concurrent.Async (race)

import qualified Data.Map.Strict as M

-- cancel

cancelTask :: Pool -> Task -> IO ()
cancelTask pool task = do
  atomically $ writeTVar (tStatus task) Cancelled

  mt <- atomically $ readTVar (tThread task)
  case mt of
    Just tid -> killThread tid
    Nothing  -> pure ()

  deps <- atomically $ readTVar (tDependents task)
  forM_ deps $ \tid -> do
    mt2 <- atomically $ do
      tasks <- readTVar (poolTasks pool)
      pure (M.lookup tid tasks)
    case mt2 of
      Just t  -> cancelTask pool t
      Nothing -> pure ()

cancel :: TaskId -> Pool -> IO ()
cancel tid pool = do
  mt <- atomically $ do
    tasks <- readTVar (poolTasks pool)
    pure (M.lookup tid tasks)

  case mt of
    Nothing -> pure ()
    Just t  -> cancelTask pool t

-- timeout

withTimeout :: Int -> IO () -> IO ()
withTimeout timeout action = do
  r <- race action (threadDelay timeout)
  case r of
    Left _  -> pure ()
    Right _ -> throwIO (userError "timeout")

-- wait

wait :: Pool -> IO ()
wait pool = atomically $ do
  tasks <- readTVar (poolTasks pool)
  sts <- mapM (readTVar . tStatus . snd) (M.toList tasks)
  unless (all done sts) retry
  where
    done s = s == Finished || s == Cancelled

waitFor :: TaskId -> Pool -> IO ()
waitFor tid pool = atomically $ do
  tasks <- readTVar (poolTasks pool)
  case M.lookup tid tasks of
    Nothing -> pure ()
    Just t -> do
      s <- readTVar (tStatus t)
      unless (s == Finished || s == Cancelled) retry
