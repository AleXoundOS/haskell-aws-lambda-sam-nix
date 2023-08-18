{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aws.Lambda as Aws
import qualified Amazonka as A
import Amazonka.DynamoDB as DynamoDB
import Amazonka.DynamoDB.ListTables as DynamoDB
import Data.Text
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Aeson.KeyMap as KeyMap
import Control.Monad.IO.Class
import Data.IORef
import Data.Time.Clock (getCurrentTime, UTCTime)
import System.IO (stdout)

-- | Toggle debug logging here.
useDebugLogging :: Bool
useDebugLogging = False

initializeContext :: IO (UTCTime, A.Env)
initializeContext = do
  env <- initEnv

  curTime <- getCurrentTime
  pure (curTime, env)
  where
    setLoggerInAwsEnv :: A.Logger -> A.Env -> A.Env
    setLoggerInAwsEnv logger env = env {A.logger = logger}
    initEnv =
      if useDebugLogging then do
        logger <- A.newLogger A.Debug stdout
        setLoggerInAwsEnv logger <$> A.newEnv A.discover
      else
        A.newEnv A.discover


main :: IO ()
main = do
  runLambdaHaskellRuntime defaultDispatcherOptions initializeContext id $ do
    addStandaloneLambdaHandler "standaloneHandler" standaloneHandler

standaloneHandler :: Value -> Aws.Context (UTCTime, A.Env)
                  -> IO (Either String Text)
standaloneHandler req awsContext = do
  (initTime, env) <- readIORef $ customContext awsContext
  tables <- getTables env

  pure $ case parseParam req of
    Nothing -> error "no \"param\" found" -- (Left values are not printed)
    Just paramTxt -> Right (text env paramTxt initTime tables)
  where
    text env paramTxt initTime tables = ""
      <> "running in region: " <> Text.pack (show $ A.region env) <> "\n"
      <> "init time: " <> Text.pack (show initTime) <> "\n"
      <> "param value: " <> paramTxt <> "\n"
      <> "existing tables in region: " <> Text.intercalate ", " tables

getTables :: A.Env -> IO [Text]
getTables env = do
  A.runResourceT $ do
    runConduit $
      A.paginate env newListTables
        .| CL.map DynamoDB.tableNames
        .| CL.catMaybes
        .| CL.concat
        .| CC.sinkList

parseParam :: Value -> Maybe Text
parseParam (Object o) =
  case KeyMap.lookup "queryStringParameters" o of
    Just (Object queryStringParameters) ->
      case KeyMap.lookup "param" queryStringParameters of
        Just (String paramTxt) -> Just paramTxt
        _ -> Nothing
    _ -> Nothing
parseParam _ = Nothing
