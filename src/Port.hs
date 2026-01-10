{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Port where

import Cmd
import Data.Bifunctor
import GHC.IO.Exception
import Types

data PortTask = PortTask
  { portHost :: String
  , port :: Int
  , portType :: PortType
  }
  deriving (Show)

data PortType = UDP | TCP
  deriving (Show)

portTask :: PortTask
portTask =
  PortTask
    { portHost = ""
    , port = 80
    , portType = TCP
    }

instance MonitoringTask PortTask where
  type TaskReponse PortTask = Bool
  check = snd . checkPort
  toRiemannEvent = portResultToRiemannEvent
  prettyCommand = fst . checkPort

checkPort :: PortTask -> (String, IO (RawOutput, Bool))
checkPort pt =
  let udp =
        case pt.portType of
          UDP -> " -u"
          _ -> ""
      netcatCall = "nc -vvv" <> udp <> " -q 5 -w 1 " <> pt.portHost <> " " <> show pt.port
   in ( netcatCall
      , fmap (second interpretResult) $
          check $
            CmdTask
              { script = netcatCall
              , inStdOut = []
              , inStdErr = expectedPortType <> [show pt.port, pt.portHost, "succeeded!"]
              , exitCode = ExitSuccess
              , expectEmptyError = False
              }
      )
  where
    expectedPortType = case pt.portType of
      UDP -> ["udp"]
      TCP -> ["tcp"]
    interpretResult = \case
      CmdOk -> True
      _ -> False

portResultToRiemannEvent :: Service -> Maybe Host -> PortTask -> Bool -> RiemannEvent
portResultToRiemannEvent service host pt True =
  RiemannOk
    { riemannService = service
    , metric = 1
    , eventHost = host
    , description = show pt.portType <> " Port " <> show pt.port <> " on " <> pt.portHost <> " is open"
    }
portResultToRiemannEvent service host pt False =
  RiemannCritical
    { riemannService = service
    , metric = 0
    , eventHost = host
    , description = show pt.portType <> " Port " <> show pt.port <> " on " <> pt.portHost <> " is not open"
    }
