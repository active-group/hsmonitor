{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Webpage where

import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.List
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import System.Process
import Types

data WebpageTask = WebpageTask
  { url :: String
  , service :: String
  , toAppear :: [BS.ByteString]
  , runHeadless :: Bool
  , basicAuth :: Maybe BS.ByteString
  , browserOptions :: [String]
  }
  deriving (Show)

webpage :: WebpageTask
webpage =
  WebpageTask
    { service = ""
    , url = ""
    , toAppear = []
    , runHeadless = False
    , basicAuth = Nothing
    , browserOptions = []
    }

instance MonitoringTask WebpageTask where
  type TaskReponse WebpageTask = WebpageResponse
  check :: WebpageTask -> IO (TaskReponse WebpageTask)
  check = getWebpage
  taskService wpt = wpt.service
  toRiemannEvent = webpageResponseToRiemannEvent

data WebpageResponse
  = WebpageOk
  | WebpageKeywordsNotFound [String]
  | WebpageError String
  deriving (Show)

withPossibleBasicAuth :: Maybe BS.ByteString -> Request -> Request
withPossibleBasicAuth Nothing = id
withPossibleBasicAuth (Just token) =
  setRequestHeader "Authorization" ["Basic " <> token]

getWebpage :: WebpageTask -> IO WebpageResponse
getWebpage webpageTask
  | webpageTask.runHeadless = getWebpageHeadless webpageTask
  | otherwise =
      do
        req <- withPossibleBasicAuth webpageTask.basicAuth <$> parseRequest webpageTask.url
        resp <- httpLBS req
        case getResponseStatus resp of
          Status 200 _ ->
            let content = LBS.toStrict $ getResponseBody resp
                missingKeywords = filter (not . (`Char8.isInfixOf` content)) webpageTask.toAppear
             in case missingKeywords of
                  [] -> pure WebpageOk
                  _ -> pure $ WebpageKeywordsNotFound (map Char8.unpack missingKeywords)
          Status _ reason -> pure $ WebpageError $ Char8.unpack reason
        `catch` ( \(e :: HttpException) ->
                    pure $
                      WebpageError $
                        show e
                )

customReadProcess :: FilePath -> [String] -> String -> IO String
customReadProcess cmd args =
  let cp = (proc cmd args){std_err = NoStream}
   in readCreateProcess cp

getWebpageHeadless :: WebpageTask -> IO WebpageResponse
getWebpageHeadless webpageTask =
  do
    let options = ["--headless", "--disable-gpu", "--dump-dom"] <> webpageTask.browserOptions <> [webpageTask.url]
    result <- Char8.pack <$> customReadProcess "chromium-browser" options ""

    let missingKeywords = filter (not . (`Char8.isInfixOf` result)) webpageTask.toAppear
    case missingKeywords of
      [] -> pure WebpageOk
      _ -> pure $ WebpageKeywordsNotFound (map Char8.unpack missingKeywords)
    `catch` (\(e :: IOError) -> pure $ WebpageError $ show e)

webpageResponseToRiemannEvent :: WebpageTask -> WebpageResponse -> RiemannEvent
webpageResponseToRiemannEvent webpageTask = \case
  WebpageOk ->
    RiemannOk
      { riemannService = webpageTask.service
      , metric = 1
      , description = "Found all expected keywords on webpage"
      }
  WebpageKeywordsNotFound missingKeywords ->
    RiemannCritical
      { riemannService = webpageTask.service
      , metric = 1 - fromIntegral (length missingKeywords) / fromIntegral (length webpageTask.toAppear)
      , description = "Failed to find the following keywords on webpage: " <> intercalate ", " missingKeywords
      }
  WebpageError reason ->
    RiemannCritical
      { riemannService = webpageTask.service
      , metric = 0
      , description = reason
      }
