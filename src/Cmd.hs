{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Cmd where

import Data.List
import GHC.IO.Exception
import System.Process
import Types

data CmdTask = CmdTask
  { service :: String
  , script :: String
  , inStdOut :: [String]
  , inStdErr :: [String]
  , exitCode :: ExitCode
  , expectEmptyError :: Bool
  }
  deriving (Show)

cmd :: CmdTask
cmd =
  CmdTask
    { service = ""
    , script = ""
    , inStdOut = []
    , inStdErr = []
    , exitCode = ExitSuccess
    , expectEmptyError = False
    }

data CmdResult
  = CmdOk
  | CmdWrongExitCode ExitCode
  | CmdMissingKeywords [String]
  | CmdStdErrorNotEmpty String
  deriving (Show)

instance MonitoringTask CmdTask where
  type TaskReponse CmdTask = CmdResult
  check = checkCmdTask
  taskService t = t.service
  toRiemannEvent = cmdToRiemannEvent

checkCmdTask :: CmdTask -> IO CmdResult
checkCmdTask command = do
  let cp = shell command.script
  (code, stdout, stderr) <- readCreateProcessWithExitCode cp ""
  if code /= command.exitCode
    then pure $ CmdWrongExitCode code
    else
      let missingKeywords =
            filter (not . (`isInfixOf` stdout)) command.inStdOut
              <> filter (not . (`isInfixOf` stderr)) command.inStdErr
       in case missingKeywords of
            []
              | command.expectEmptyError, not (null stderr) -> pure $ CmdStdErrorNotEmpty stderr
              | otherwise -> pure CmdOk
            _ -> pure $ CmdMissingKeywords missingKeywords

cmdToRiemannEvent :: CmdTask -> CmdResult -> RiemannEvent
cmdToRiemannEvent ct = \case
  CmdOk ->
    RiemannOk
      { riemannService = ct.service
      , metric = 1
      , description = "Command " <> ct.script <> " was successful"
      }
  CmdWrongExitCode code ->
    RiemannCritical
      { riemannService = ct.service
      , metric = 0
      , description = "Command " <> ct.script <> " exited with wrong exit code. Expected " <> show ct.exitCode <> " but got " <> show code
      }
  CmdMissingKeywords keywords ->
    RiemannCritical
      { riemannService = ct.service
      , metric = 1 - fromIntegral (length keywords) / fromIntegral (length ct.inStdErr + length ct.inStdOut)
      , description = "Command " <> ct.script <> " output is missing the keywords: " <> intercalate ", " keywords
      }
  CmdStdErrorNotEmpty stderr ->
    RiemannCritical
      { riemannService = ct.service
      , metric = 0
      , description = "Command " <> ct.script <> " stderr output was not empty: " <> stderr
      }
