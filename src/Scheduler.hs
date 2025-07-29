{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Scheduler where

import Control.Concurrent
import Control.Concurrent.Async
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
createJob cfg t@Task{..}
  | timeout > interval = do
      putStrLn $
        "Task "
          <> show service
          <> " has a timeout higher than interval ("
          <> show timeout
          <> " > "
          <> show interval
          <> " ). Reducing timeout to "
          <> show interval
          <> "."
      createJob cfg t{timeout = t.interval}
  | otherwise = do
      case cfg.startupDelay of
        Just maxStartupDelay | maxStartupDelay > 0 -> do
          startupDelay <- microsecondsToNominalDiffTime <$> randomRIO (0, nominalDiffTimeToMicroseconds maxStartupDelay)
          delayBy startupDelay
        _ -> pure ()
      now <- getCurrentTime
      go now
  where
    go lastExec = do
      execTask service host timeout cfg checkTask
      (delay, newLastExec) <- delayToNextExec lastExec interval
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
      sendToRiemann cfg $
        RiemannCritical
          { riemannService = service
          , metric = 0
          , eventHost = host
          , description = "Timeout reached: " <> show timeout
          }
    Right response -> sendToRiemann cfg $ toRiemannEvent service host t response
