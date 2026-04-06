{-# LANGUAGE ScopedTypeVariables #-}

module Tests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Types
import Pool
import API
import Control

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Task Scheduler Tests"
  [ basicTests
  , dependencyTests
  , priorityTests
  , coreTests
  , cancelTests
  , timeoutTests
  ]

--------------------------------------------------------------------------------
-- BASIC

basicTests :: TestTree
basicTests = testGroup "Basic"
  [ testCase "runs single task" $ do
      pool <- newPool 2
      v <- newEmptyMVar

      _ <- submit 1 1 (liftIO $ putMVar v ()) [] pool
      wait pool

      r <- tryTakeMVar v
      r @?= Just ()

  , testCase "runs many tasks" $ do
      pool <- newPool 4
      c <- newTVarIO (0 :: Int)

      let task = atomically $ modifyTVar' c (+1)

      replicateM_ 10 $
        submit 1 1 (liftIO task) [] pool

      wait pool

      res <- atomically $ readTVar c
      res @?= 10
  ]

--------------------------------------------------------------------------------
-- DEPENDENCIES

dependencyTests :: TestTree
dependencyTests = testGroup "Dependencies"
  [ testCase "chain execution" $ do
      pool <- newPool 2
      order <- newMVar []

      t1 <- submit 1 1 (liftIO $
              modifyMVar_ order (\xs -> pure (xs ++ [1]))) [] pool

      t2 <- submit 1 1 (liftIO $
              modifyMVar_ order (\xs -> pure (xs ++ [2]))) [t1] pool

      _  <- submit 1 1 (liftIO $
              modifyMVar_ order (\xs -> pure (xs ++ [3]))) [t2] pool

      wait pool

      res <- readMVar order
      res @?= [1,2,3]

  , testCase "multiple dependencies" $ do
      pool <- newPool 2
      order <- newMVar []

      t1 <- submit 1 1 (liftIO $
              modifyMVar_ order (\xs -> pure (xs ++ [1]))) [] pool

      t2 <- submit 1 1 (liftIO $
              modifyMVar_ order (\xs -> pure (xs ++ [2]))) [] pool

      _ <- submit 1 1 (liftIO $
              modifyMVar_ order (\xs -> pure (xs ++ [3]))) [t1, t2] pool

      wait pool

      res <- readMVar order
      last res @?= 3
  ]

--------------------------------------------------------------------------------
-- PRIORITY

priorityTests :: TestTree
priorityTests = testGroup "Priority"
  [ testCase "higher priority first" $ do
      pool <- newPool 1
      order <- newMVar []

      _ <- submit 1 1 (liftIO $
            threadDelay 100000 >>
            modifyMVar_ order (\xs -> pure (xs ++ [1]))) [] pool

      _ <- submit 10 1 (liftIO $
            modifyMVar_ order (\xs -> pure (xs ++ [2]))) [] pool

      wait pool

      res <- readMVar order
      res @?= [2,1]
  ]

--------------------------------------------------------------------------------
-- CORES

coreTests :: TestTree
coreTests = testGroup "Cores"
  [ testCase "respects core limit" $ do
      pool <- newPool 2

      running <- newTVarIO (0 :: Int)
      maxSeen <- newTVarIO (0 :: Int)

      let task = do
            atomically $ do
              modifyTVar' running (+1)
              r <- readTVar running
              modifyTVar' maxSeen (max r)

            threadDelay 100000

            atomically $ modifyTVar' running (subtract 1)

      replicateM_ 5 $
        submit 1 1 (liftIO task) [] pool

      wait pool

      m <- atomically $ readTVar maxSeen
      m @?= 2
  ]

--------------------------------------------------------------------------------
-- CANCEL

cancelTests :: TestTree
cancelTests = testGroup "Cancel"
  [ testCase "cancel running task" $ do
      pool <- newPool 2
      v <- newEmptyMVar

      t <- submit 1 1 (liftIO $
            threadDelay 1000000 >> putMVar v ()) [] pool

      cancel t pool
      wait pool

      r <- tryTakeMVar v
      r @?= Nothing

  , testCase "cascade cancel" $ do
      pool <- newPool 2
      v <- newEmptyMVar

      t1 <- submit 1 1 (liftIO $ threadDelay 1000000) [] pool


      _  <- submit 1 1 (liftIO $ putMVar v ()) [t1] pool

      cancel t1 pool
      wait pool

      r <- tryTakeMVar v
      r @?= Nothing
  ]

--------------------------------------------------------------------------------
-- TIMEOUT

timeoutTests :: TestTree
timeoutTests = testGroup "Timeout"
  [ testCase "timeout does not block scheduler" $ do
      pool <- newPool 1
      v <- newEmptyMVar

      _ <- submit 1 1 (liftIO $
            withTimeout 100000 (threadDelay 1000000)) [] pool

      _ <- submit 1 1 (liftIO $ putMVar v ()) [] pool

      wait pool

      r <- tryTakeMVar v
      r @?= Just ()
  ]
