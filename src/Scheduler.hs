{-# LANGUAGE OverloadedRecordDot #-}

module Scheduler where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Time
import Riemann
import System.Random
import Types

runSchedule :: Config -> MonitoringConfig -> IO ()
runSchedule cfg (MonitoringConfig tasks) = do
  putStrLn $ "Running schedule with: " <> show (length tasks) <> " tasks"
  mapConcurrently_ (createJob cfg) tasks

createJob :: Config -> (Int, Task) -> IO ()
createJob cfg (taskIntervalMinutes, Task t) = do
  when cfg.startupDelay $ do
    startupDelay <- (1_000_000 *) <$> randomRIO (0, 60)
    threadDelay startupDelay
  now <- getCurrentTime
  go now
  where
    taskInterval = fromIntegral $ taskIntervalMinutes * 60

    go lastExec = do
      execTask cfg t
      (delay, newLastExec) <- first ((1_000_000 *) . round) <$> delayToNextExec lastExec taskInterval
      threadDelay delay
      go newLastExec

delayToNextExec :: UTCTime -> NominalDiffTime -> IO (NominalDiffTime, UTCTime)
delayToNextExec lastExecTime interval = do
  let nextExecTime = addUTCTime interval lastExecTime
  delay <- diffUTCTime nextExecTime <$> getCurrentTime
  pure (delay, nextExecTime)

execTask :: (MonitoringTask t) => Config -> t -> IO ()
execTask cfg t = do
  res <-
    race
      (threadDelay (1_000_000 * round (timeout t)))
      (check t)

  case res of
    Left () ->
      print $
        RiemannCritical
          { riemannService = taskService t
          , metric = 0
          , description = "Timeout reached: " <> show (timeout t)
          }
    Right response -> sendToRiemann cfg $ toRiemannEvent t response
