{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Scheduler where

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

delayBy :: NominalDiffTime -> IO ()
delayBy = threadDelay . nominalDiffTimeToMicroseconds

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
nominalDiffTimeToMicroseconds = round . (1_000_000 *) . nominalDiffTimeToSeconds

microsecondsToNominalDiffTime :: Int -> NominalDiffTime
microsecondsToNominalDiffTime = secondsToNominalDiffTime . (/ 1_000_000) . fromIntegral

createJob :: Config -> Task -> IO ()
createJob cfg Task{..} = do
  when (cfg.startupDelay > 0) $ do
    startupDelay <- microsecondsToNominalDiffTime <$> randomRIO (0, nominalDiffTimeToMicroseconds cfg.startupDelay)
    delayBy startupDelay
  now <- getCurrentTime
  go now
  where
    taskInterval = fromIntegral $ interval * 60

    go lastExec = do
      execTask service host timeout cfg checkTask
      (delay, newLastExec) <- delayToNextExec lastExec taskInterval
      delayBy delay
      go newLastExec

delayToNextExec :: UTCTime -> NominalDiffTime -> IO (NominalDiffTime, UTCTime)
delayToNextExec lastExecTime interval = do
  let nextExecTime = addUTCTime interval lastExecTime
  delay <- diffUTCTime nextExecTime <$> getCurrentTime
  pure (delay, nextExecTime)

execTask :: (MonitoringTask t) => Service -> Maybe Host -> NominalDiffTime -> Config -> t -> IO ()
execTask service host timeout cfg t = do
  res <-
    race
      (delayBy timeout)
      (check t)

  case res of
    Left () ->
      print $
        RiemannCritical
          { riemannService = service
          , metric = 0
          , eventHost = host
          , description = "Timeout reached: " <> show timeout
          }
    Right response -> sendToRiemann cfg $ toRiemannEvent service host t response
