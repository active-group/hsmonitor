{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Cmd where

import Data.List
import GHC.IO.Exception
import System.Process
import Types

data CmdTask = CmdTask
  { script :: String
  , inStdOut :: [String]
  , inStdErr :: [String]
  , exitCode :: ExitCode
  , expectEmptyError :: Bool
  }
  deriving (Show)

cmd :: CmdTask
cmd =
  CmdTask
    { script = ""
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

cmdToRiemannEvent :: Service -> Maybe Host -> CmdTask -> CmdResult -> RiemannEvent
cmdToRiemannEvent service host ct = \case
  CmdOk ->
    RiemannOk
      { riemannService = service
      , metric = 1
      , eventHost = host
      , description = "Command " <> ct.script <> " was successful"
      }
  CmdWrongExitCode code ->
    RiemannCritical
      { riemannService = service
      , metric = 0
      , eventHost = host
      , description = "Command " <> ct.script <> " exited with wrong exit code. Expected " <> show ct.exitCode <> " but got " <> show code
      }
  CmdMissingKeywords keywords ->
    RiemannCritical
      { riemannService = service
      , metric = 1 - fromIntegral (length keywords) / fromIntegral (length ct.inStdErr + length ct.inStdOut)
      , eventHost = host
      , description = "Command " <> ct.script <> " output is missing the keywords: " <> intercalate ", " keywords
      }
  CmdStdErrorNotEmpty stderr ->
    RiemannCritical
      { riemannService = service
      , metric = 0
      , eventHost = host
      , description = "Command " <> ct.script <> " stderr output was not empty: " <> stderr
      }
