module Main where

import Config qualified
import Options
import Scheduler qualified

main :: IO ()
main = do
  cfg <- exec Config.cfg
  putStrLn "Starting up monitoring ..."
  putStrLn $ "Using " <> show cfg
  Scheduler.runSchedule cfg Config.schedule
