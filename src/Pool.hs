module Pool where

import Types
import Scheduler

import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Map.Strict as M

newPool :: Int -> IO Pool
newPool cores = do
  tasks <- newTVarIO M.empty
  next  <- newTVarIO 0
  ready <- newTVarIO mempty
  free  <- newTVarIO cores

  let pool = Pool tasks next ready free

  _ <- forkIO (scheduler pool)

  pure pool

newId :: Pool -> STM TaskId
newId pool = do
  i <- readTVar (poolNextId pool)
  writeTVar (poolNextId pool) (i + 1)
  pure (TaskId i)
