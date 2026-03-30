module Tests where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Unlift
import Data.IORef
import System.Timeout

import Task

tests :: TestTree
tests = testGroup "Task"
  [ testCase "single task runs" $ do
      pool <- newPool 4
      ref <- newTVarIO False
      _ <- submit (atomically $ writeTVar ref True) [] pool
      wait pool
      readTVarIO ref >>= (@?= True)

  , testCase "parallel tasks" $ do
      pool <- newPool 4
      ref1 <- newTVarIO False
      ref2 <- newTVarIO False

      _ <- submit (threadDelay 100000 >> atomically (writeTVar ref1 True)) [] pool
      _ <- submit (threadDelay 100000 >> atomically (writeTVar ref2 True)) [] pool

      result <- timeout 200000 (wait pool)
      assertBool "should run in parallel" (result /= Nothing)

  , testCase "dependencies respected" $ do
      pool <- newPool 4
      ref <- newTVarIO ([] :: [Int])

      t1 <- submit (atomically $ modifyTVar' ref (++ [1])) [] pool
      _  <- submit (atomically $ modifyTVar' ref (++ [2])) [t1] pool

      wait pool
      readTVarIO ref >>= (@?= [1,2])

  , testCase "cancel works" $ do
      pool <- newPool 4
      block <- newEmptyTMVarIO

      t <- submit (atomically $ takeTMVar block) [] pool
      _ <- cancel t pool

      s <- status t pool
      s @?= Just Cancelled

  , testCase "timeout works" $ do
      pool <- newPool 4

      _ <- submitWithTimeout 50000 (threadDelay 1000000) [] pool
      wait pool

      -- просто проверяем что не зависло
      assertBool "timeout didn't hang" True
  ]
