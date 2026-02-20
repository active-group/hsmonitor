{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

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

type Command = String

type RawOutput = [String]

data CommandResponse t
  = CommandResponse
  { command :: Command
  , response :: IO (RawOutput, TaskReponse t)
  }

transformResponse :: (TaskReponse t -> TaskReponse t') -> CommandResponse t -> CommandResponse t'
transformResponse f cr = CommandResponse cr.command (fmap (fmap f) cr.response)

class MonitoringTask t where
  type TaskReponse t
  internalTimeout :: t -> Maybe NominalDiffTime
  internalTimeout _ = Nothing
  check :: t -> CommandResponse t
  toRiemannEvent :: Service -> Maybe Host -> t -> TaskReponse t -> RiemannEvent

instance MonitoringTask RiemannEvent where
  type TaskReponse RiemannEvent = RiemannEvent
  check re = CommandResponse "" (pure $ ([], re))
  toRiemannEvent _ _ _ = id

type Service = String

type Host = String

data Task where
  Task ::
    (MonitoringTask t) =>
    { interval :: NominalDiffTime
    , service :: Service
    , host :: Maybe Host
    , timeout :: NominalDiffTime
    , checkTask :: t
    } ->
    Task

task :: (MonitoringTask t) => Service -> t -> Task
task service t =
  Task
    { interval = 60
    , service = service
    , host = Nothing
    , timeout = maybe 10 (max 10) $ internalTimeout t
    , checkTask = t
    }

data RiemannConfig = RiemannConfig String Int
  deriving (Show)

data Config = Config
  { riemannConfig :: Maybe RiemannConfig
  , startupDelay :: Maybe NominalDiffTime
  , debug :: Bool
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { riemannConfig = Nothing
    , startupDelay = Nothing
    , debug = False
    }
