{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Port where

import Cmd
import GHC.IO.Exception
import Types

data PortTask = PortTask
  { service :: String
  , host :: String
  , port :: Int
  , portType :: PortType
  }
  deriving (Show)

data PortType = UDP | TCP
  deriving (Show)

portTask :: PortTask
portTask =
  PortTask
    { Port.service = ""
    , host = ""
    , port = 80
    , portType = TCP
    }

instance MonitoringTask PortTask where
  type TaskReponse PortTask = Bool
  check = checkPort
  taskService t = t.service
  toRiemannEvent = portResultToRiemannEvent

checkPort :: PortTask -> IO Bool
checkPort pt =
  let udp =
        case pt.portType of
          UDP -> " -u"
          _ -> ""
   in fmap interpretResult $
        check $
          CmdTask
            { Cmd.service = pt.service
            , script = "nc -vvv" <> udp <> " -q 5 -w 1 " <> pt.host <> " " <> show pt.port
            , inStdOut = []
            , inStdErr = expectedPortType <> [show pt.port, pt.host, "succeeded!"]
            , exitCode = ExitSuccess
            , expectEmptyError = False
            }
  where
    expectedPortType = case pt.portType of
      UDP -> ["udp"]
      TCP -> ["tcp"]
    interpretResult = \case
      CmdOk -> True
      _ -> False

portResultToRiemannEvent :: PortTask -> Bool -> RiemannEvent
portResultToRiemannEvent pt True =
  RiemannOk
    { riemannService = pt.service
    , metric = 1
    , description = show pt.portType <> " Port " <> show pt.port <> " on " <> pt.host <> " is open"
    }
portResultToRiemannEvent pt False =
  RiemannCritical
    { riemannService = pt.service
    , metric = 0
    , description = show pt.portType <> " Port " <> show pt.port <> " on " <> pt.host <> " is not open"
    }
