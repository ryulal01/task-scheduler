module Main where

import Task
import Control.Monad
import Control.Concurrent
import Data.Time.Clock

main :: IO ()
main = do
  pool <- newPool 4

  start <- getCurrentTime

  tids <- forM [1..1000] $ \_ ->
    submit (threadDelay 1000) [] pool

  mapM_ (`waitFor` pool) tids

  end <- getCurrentTime

  putStrLn "Benchmark:"
  print (diffUTCTime end start)
