module Types where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Time.Clock

import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Max as PQ

newtype TaskId = TaskId Int deriving (Eq, Ord, Show)

data Status = Pending | Running | Finished | Cancelled
  deriving (Eq, Show)

data Task = Task
  { tId         :: TaskId
  , tAction     :: IO ()
  , tDeps       :: [TaskId]
  , tDependents :: TVar [TaskId]
  , tStatus     :: TVar Status
  , tThread     :: TVar (Maybe ThreadId)
  , tResult     :: TVar (Maybe (Either SomeException ()))

  -- scheduler fields
  , tPriority   :: Int
  , tCores      :: Int
  , tCreated    :: UTCTime   -- для aging
  }

data Pool = Pool
  { poolTasks      :: TVar (M.Map TaskId Task)
  , poolNextId     :: TVar Int

  , poolReady      :: TVar (PQ.MaxPQueue Int TaskId)
  , poolFreeCores  :: TVar Int
  }
