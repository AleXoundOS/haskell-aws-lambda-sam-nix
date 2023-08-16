{-# LANGUAGE OverloadedStrings #-}
module Main where

import Aws.Lambda
import Data.Text
import qualified Data.Text as Text
import GHC.Generics
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe

main :: IO ()
main =
  runLambdaHaskellRuntime defaultDispatcherOptions (pure ()) id $ do
    addStandaloneLambdaHandler "standaloneHandler" standaloneHandler

standaloneHandler :: Value -> Context () -> IO (Either String Text)
standaloneHandler req _context = do
  print req -- this gets printed in CloudWatch logs
  pure $ case parseParam req of
    Nothing -> error "no \"param\" found" -- (Left values are not printed)
    Just paramTxt -> Right paramTxt

parseParam :: Value -> Maybe Text
parseParam (Object o) =
  case KeyMap.lookup "queryStringParameters" o of
    Just (Object queryStringParameters) ->
      case KeyMap.lookup "param" queryStringParameters of
        Just (String paramTxt) -> Just paramTxt
        _ -> Nothing
    _ -> Nothing
parseParam _ = Nothing
