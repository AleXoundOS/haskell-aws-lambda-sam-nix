{-# LANGUAGE OverloadedStrings #-}
module Main where

import Aws.Lambda
import Data.Text
import qualified Data.Text as Text
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import Data.Time.Clock (getCurrentTime, UTCTime)

main :: IO ()
main =
  runLambdaHaskellRuntime defaultDispatcherOptions getCurrentTime id $ do
    addStandaloneLambdaHandler "standaloneHandler" standaloneHandler

standaloneHandler :: Value -> Context UTCTime -> IO (Either String Text)
standaloneHandler req awsContext = do
  context <- readIORef $ customContext awsContext
  print req -- this gets printed in CloudWatch logs
  pure $ case parseParam req of
    Nothing -> error "no \"param\" found" -- (Left values are not printed)
    Just paramTxt -> Right (Text.pack (show context) <> "\n" <> paramTxt)

parseParam :: Value -> Maybe Text
parseParam (Object o) =
  case KeyMap.lookup "queryStringParameters" o of
    Just (Object queryStringParameters) ->
      case KeyMap.lookup "param" queryStringParameters of
        Just (String paramTxt) -> Just paramTxt
        _ -> Nothing
    _ -> Nothing
parseParam _ = Nothing
