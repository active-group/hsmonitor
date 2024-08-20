{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Mail where

import Control.Exception
import Data.String
import Data.Time
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP qualified as Mail
import Types

data Relay = Relay
  { host :: String
  , port :: Int
  , username :: String
  , password :: String
  }
  deriving (Show)

data MailTask = MailTask
  { sender :: String
  , receiver :: String
  , mailContent :: String
  , relay :: Relay
  }
  deriving (Show)

mail :: MailTask
mail =
  MailTask
    { sender = ""
    , receiver = ""
    , mailContent = ""
    , relay =
        Relay
          { host = ""
          , port = 25
          , username = ""
          , password = ""
          }
    }

data MailResponse
  = MailSendSuccess
  | MailSendFailure String
  deriving (Show)

instance MonitoringTask MailTask where
  type TaskReponse MailTask = MailResponse
  check = checkMail
  toRiemannEvent = mailReponseToRiemannEvent

mailReponseToRiemannEvent :: Service -> Maybe Host -> MailTask -> MailResponse -> RiemannEvent
mailReponseToRiemannEvent service host mt = \case
  MailSendSuccess ->
    RiemannOk
      { riemannService = service
      , metric = 1
      , eventHost = host
      , description = "Sending mail from '" <> mt.sender <> "' to '" <> mt.receiver <> "' successful"
      }
  MailSendFailure err ->
    RiemannCritical
      { riemannService = service
      , metric = 0
      , eventHost = host
      , description =
          "Sending mail from '" <> mt.sender <> "' to '" <> mt.receiver <> "' failed, response was: '" <> err <> "'"
      }

checkMail :: MailTask -> IO MailResponse
checkMail mt =
  do
    now <- getCurrentTime
    let date = formatTime defaultTimeLocale "%F %T" now

    let email =
          Mail.simpleMail
            (fromString mt.sender)
            [fromString mt.receiver]
            []
            []
            (fromString $ "HSMonitor test mail " <> date)
            [Mime.plainPart $ fromString $ "Date: " <> date <> "\r\n" <> mt.mailContent]

    (MailSendSuccess <$ Mail.sendMailWithLoginSTARTTLS' mt.relay.host (fromIntegral mt.relay.port) mt.relay.username mt.relay.password email)
      `catch` ( \(e :: SomeException) ->
                  pure $ MailSendFailure $ show e
              )
