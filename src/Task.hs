module Task where

import Text.Blaze.Html
import Text.Blaze.Html5 as H

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.QSem
import Control.Concurrent.Async (race)
import Control.Exception (SomeException, handle, try, throwIO)

import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M

-- TaskId
newtype TaskId = TaskId Int deriving (Eq, Ord, Show)

data Status = Pending | Running | Finished | Cancelled deriving (Eq, Show)

data Task = Task
  { tId :: TaskId
  , tAction :: IO ()
  , tDeps :: [TaskId]
  , tDependents :: TVar [TaskId]
  , tStatus :: TVar Status
  , tThread :: TVar (Maybe ThreadId)
  , tResult :: TVar (Maybe (Either SomeException ()))
  }

data Pool = Pool
  { poolTasks :: TVar (M.Map TaskId Task)
  , poolNextId :: TVar Int
  , poolSem :: QSem
  }

-- create pool with limit
newPool :: MonadIO m => Int -> m Pool
newPool maxThreads = liftIO $ do
  tasks <- newTVarIO M.empty
  next  <- newTVarIO 0
  sem   <- newQSem maxThreads
  pure $ Pool tasks next sem

newId :: Pool -> STM TaskId
newId pool = do
  i <- readTVar (poolNextId pool)
  writeTVar (poolNextId pool) (i + 1)
  pure (TaskId i)

lookupTask :: TaskId -> M.Map TaskId Task -> Maybe Task
lookupTask = M.lookup

depsFinished :: Pool -> [TaskId] -> STM Bool
depsFinished pool deps = do
  tasks <- readTVar (poolTasks pool)
  and <$> mapM (check tasks) deps
  where
    check tasks tid =
      case lookupTask tid tasks of
        Nothing -> pure True
        Just t -> (== Finished) <$> readTVar (tStatus t)

startTask :: Pool -> Task -> IO ()
startTask pool task = do
  waitQSem (poolSem pool)

  atomically $ writeTVar (tStatus task) Running

  tid <- forkIO $ do
    res <- try (tAction task)
    atomically $ writeTVar (tResult task) (Just res)
    finishTask pool task
    signalQSem (poolSem pool)

  atomically $ writeTVar (tThread task) (Just tid)

finishTask :: Pool -> Task -> IO ()
finishTask pool task = do
  atomically $ writeTVar (tStatus task) Finished

  deps <- atomically $ readTVar (tDependents task)

  forM_ deps $ \tid -> do
    mt <- atomically $ do
      tasks <- readTVar (poolTasks pool)
      pure (lookupTask tid tasks)

    case mt of
      Nothing -> pure ()
      Just t -> do
        ready <- atomically $ depsFinished pool (tDeps t)
        when ready $ startTask pool t

submit :: MonadUnliftIO m => m () -> [TaskId] -> Pool -> m TaskId
submit action deps pool = withRunInIO $ \run -> do

  tid <- atomically $ newId pool

  statusVar <- newTVarIO Pending
  threadVar <- newTVarIO Nothing
  dependentsVar <- newTVarIO []
  resultVar <- newTVarIO Nothing

  let task = Task
        tid
        (run action)
        deps
        dependentsVar
        statusVar
        threadVar
        resultVar

  atomically $ do
    modifyTVar' (poolTasks pool) (M.insert tid task)

    tasks <- readTVar (poolTasks pool)

    forM_ deps $ \d ->
      case lookupTask d tasks of
        Nothing -> pure ()
        Just depTask ->
          modifyTVar' (tDependents depTask) (tid:)

  ready <- atomically $ depsFinished pool deps
  when ready $ startTask pool task

  pure tid

-- ✅ FIXED TIMEOUT
submitWithTimeout
  :: MonadUnliftIO m
  => Int
  -> m ()
  -> [TaskId]
  -> Pool
  -> m TaskId
submitWithTimeout timeout action deps pool =
  withRunInIO $ \run -> do
    let wrapped = do
          r <- race (run action) (threadDelay timeout)
          case r of
            Left _  -> pure ()
            Right _ -> throwIO (userError "timeout")
    submit (liftIO wrapped) deps pool

data CancelStatus = Success | AlreadyFinished | AlreadyCancelled | NotPresent
  deriving (Eq, Show)

cancelTask :: Pool -> Task -> IO ()
cancelTask pool task = do
  atomically $ writeTVar (tStatus task) Cancelled

  mt <- atomically $ readTVar (tThread task)
  case mt of
    Nothing -> pure ()
    Just tid -> killThread tid

  deps <- atomically $ readTVar (tDependents task)

  forM_ deps $ \tid -> do
    mt2 <- atomically $ do
      tasks <- readTVar (poolTasks pool)
      pure (lookupTask tid tasks)
    case mt2 of
      Nothing -> pure ()
      Just t -> cancelTask pool t

cancel :: MonadIO m => TaskId -> Pool -> m CancelStatus
cancel tid pool = liftIO $ do
  mt <- atomically $ do
    tasks <- readTVar (poolTasks pool)
    pure (lookupTask tid tasks)

  case mt of
    Nothing -> pure NotPresent
    Just t -> do
      st <- atomically $ readTVar (tStatus t)
      case st of
        Finished -> pure AlreadyFinished
        Cancelled -> pure AlreadyCancelled
        _ -> cancelTask pool t >> pure Success

status :: MonadIO m => TaskId -> Pool -> m (Maybe Status)
status tid pool = liftIO . atomically $ do
  tasks <- readTVar (poolTasks pool)
  case lookupTask tid tasks of
    Nothing -> pure Nothing
    Just t -> Just <$> readTVar (tStatus t)

tasksWith :: Pool -> Status -> IO [TaskId]
tasksWith pool st = atomically $ do
  tasks <- readTVar (poolTasks pool)
  fmap catMaybes $ forM (M.toList tasks) $ \(tid, t) -> do
    s <- readTVar (tStatus t)
    pure $ if s == st then Just tid else Nothing

running pool = liftIO $ tasksWith pool Running
pending pool = liftIO $ tasksWith pool Pending
finished pool = liftIO $ tasksWith pool Finished
cancelled pool = liftIO $ tasksWith pool Cancelled

wait :: MonadIO m => Pool -> m ()
wait pool = liftIO . atomically $ do
  tasks <- readTVar (poolTasks pool)
  statuses <- mapM (readTVar . tStatus . snd) (M.toList tasks)
  unless (all (\s -> s == Finished || s == Cancelled) statuses) retry

waitFor :: MonadIO m => TaskId -> Pool -> m ()
waitFor tid pool = liftIO . atomically $ do
  tasks <- readTVar (poolTasks pool)
  case lookupTask tid tasks of
    Nothing -> pure ()
    Just t -> do
      s <- readTVar (tStatus t)
      unless (s == Finished || s == Cancelled) retry

renderStatusPage :: MonadIO m => Pool -> m Html
renderStatusPage pool = liftIO $ do
  tasks <- atomically $ readTVar (poolTasks pool)

  rows <- forM (M.toList tasks) $ \(tid, t) -> do
    s <- atomically $ readTVar (tStatus t)
    pure $
      H.tr $ do
        H.td (toHtml (show tid))
        H.td (toHtml (show s))

  pure $
    H.html $ do
      H.body $ do
        H.h1 (toHtml "Task Pool Status")
        H.table $ do
          H.tr $ do
            H.th (toHtml "TaskId")
            H.th (toHtml "Status")
          sequence_ rows
