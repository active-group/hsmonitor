{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Time

newtype MonitoringConfig = MonitoringConfig {monitoringTasks :: [Task]}

data RiemannEvent
  = RiemannOk
      { riemannService :: String
      , metric :: Double
      , eventHost :: Maybe Host
      , description :: String
      }
  | RiemannCritical
      { riemannService :: String
      , metric :: Double
      , eventHost :: Maybe Host
      , description :: String
      }
  deriving (Show)

class MonitoringTask t where
  type TaskReponse t
  check :: t -> IO (TaskReponse t)
  toRiemannEvent :: Service -> Maybe Host -> t -> TaskReponse t -> RiemannEvent

type Service = String

type Host = String

data Task where
  Task ::
    (MonitoringTask t) =>
    { interval :: Int
    , service :: Service
    , host :: Maybe Host
    , timeout :: NominalDiffTime
    , checkTask :: t
    } ->
    Task

task :: (MonitoringTask t) => Service -> t -> Task
task service t =
  Task
    { interval = 1
    , service = service
    , host = Nothing
    , timeout = 10
    , checkTask = t
    }

data Config = Config
  { riemannHost :: String
  , riemannPort :: Int
  , startupDelay :: Maybe NominalDiffTime
  }
  deriving (Show)
