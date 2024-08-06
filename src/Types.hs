{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Time

newtype MonitoringConfig = MonitoringConfig {monitoringTasks :: [(Int, Task)]}

data RiemannEvent
  = RiemannOk
      { riemannService :: String
      , metric :: Double
      , description :: String
      }
  | RiemannCritical
      { riemannService :: String
      , metric :: Double
      , description :: String
      }
  deriving (Show)

class MonitoringTask t where
  type TaskReponse t
  check :: t -> IO (TaskReponse t)
  taskService :: t -> String
  toRiemannEvent :: t -> TaskReponse t -> RiemannEvent
  timeout :: t -> NominalDiffTime
  timeout _ = 10

data Task where
  Task :: (MonitoringTask t) => t -> Task

data Config = Config
  { riemannHost :: String
  , riemannPort :: Int
  , startupDelay :: Bool
  }
  deriving (Show)
