{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Mail where

import Codec.Binary.UTF8.String qualified as UTF8
import Control.Exception
import Data.String
import Data.Time
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP qualified as Mail
import Types

data Server = Server
  { mailHost :: String
  , mailPort :: Int
  , username :: String
  , password :: String
  }
  deriving (Show)

data MailTask = MailTask
  { sender :: String
  , receiver :: String
  , mailContent :: String
  , server :: Server
  }
  deriving (Show)

mail :: MailTask
mail =
  MailTask
    { sender = ""
    , receiver = ""
    , mailContent = ""
    , server =
        Server
          { mailHost = ""
          , mailPort = 25
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

checkMail :: MailTask -> CommandResponse MailTask
checkMail mt =
  let email date =
        Mail.simpleMail
          (fromString mt.sender)
          [fromString mt.receiver]
          []
          []
          (fromString $ "HSMonitor test mail " <> date)
          [Mime.plainPart $ fromString $ "Date: " <> date <> "\r\n" <> mt.mailContent]

      runCheck = do
        now <- getCurrentTime
        let date = formatTime defaultTimeLocale "%F %T" now

        ( ([], MailSendSuccess)
            <$ Mail.sendMailWithLoginSTARTTLS'
              mt.server.mailHost
              (fromIntegral mt.server.mailPort)
              mt.server.username
              (UTF8.encodeString mt.server.password)
              (email date)
          )
          `catch` ( \(e :: SomeException) ->
                      pure ([], MailSendFailure $ show e)
                  )
   in CommandResponse (show $ email "CURRENT DATE") runCheck
