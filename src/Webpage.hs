{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Webpage where

import Codec.Binary.UTF8.String qualified as UTF8
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
  , toAppear :: [BS.ByteString]
  , runHeadless :: Bool
  , basicAuth :: Maybe (String, String)
  , browserOptions :: [String]
  }
  deriving (Show)

webpage :: WebpageTask
webpage =
  WebpageTask
    { url = ""
    , toAppear = []
    , runHeadless = False
    , basicAuth = Nothing
    , browserOptions = []
    }

instance MonitoringTask WebpageTask where
  type TaskReponse WebpageTask = WebpageResponse
  check :: WebpageTask -> IO (TaskReponse WebpageTask)
  check = getWebpage
  toRiemannEvent = webpageResponseToRiemannEvent

data WebpageResponse
  = WebpageOk
  | WebpageKeywordsNotFound [String]
  | WebpageError String
  deriving (Show)

withPossibleBasicAuth :: Maybe (String, String) -> Request -> Request
withPossibleBasicAuth Nothing = id
withPossibleBasicAuth (Just (username, password)) =
  let encode = Char8.pack . UTF8.encodeString
   in setRequestBasicAuth (encode username) (encode password)

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

webpageResponseToRiemannEvent :: Service -> Maybe Host -> WebpageTask -> WebpageResponse -> RiemannEvent
webpageResponseToRiemannEvent service host webpageTask = \case
  WebpageOk ->
    RiemannOk
      { riemannService = service
      , metric = 1
      , eventHost = host
      , description = "Found all expected keywords on webpage"
      }
  WebpageKeywordsNotFound missingKeywords ->
    RiemannCritical
      { riemannService = service
      , metric = 1 - fromIntegral (length missingKeywords) / fromIntegral (length webpageTask.toAppear)
      , eventHost = host
      , description = "Failed to find the following keywords on webpage: " <> intercalate ", " missingKeywords
      }
  WebpageError reason ->
    RiemannCritical
      { riemannService = service
      , metric = 0
      , eventHost = host
      , description = reason
      }
