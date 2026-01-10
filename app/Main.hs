{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Config qualified
import Control.Monad
import Data.Maybe
import Options
import Scheduler qualified
import Types

main :: IO ()
main = do
  cfg <- exec Config.cfg
  putStrLn "Starting up monitoring ..."
  putStrLn $ "Using " <> show cfg
  when (isNothing cfg.riemannConfig) $
    putStrLn "No riemann config present, events will only be printed to stdout"
  Scheduler.runSchedule cfg Config.schedule
