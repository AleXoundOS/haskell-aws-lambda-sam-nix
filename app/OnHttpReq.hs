{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aws.Lambda as Aws
import qualified Amazonka as A
import Amazonka.DynamoDB as D
import qualified Amazonka.DynamoDB.Query as D
import qualified Amazonka.DynamoDB.Types.AttributeValue as DA
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import System.IO (stdout)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)


type TableName = Text
type Item = HashMap Text AttributeValue

type UUID = Text

newtype QueryParams = QueryParams UUID
  deriving Show

instance FromJSON QueryParams where
  parseJSON (Object o) = QueryParams <$> o .: "uuid"
  parseJSON _ = mempty

-- | Toggle debug logging here.
useDebugLogging :: Bool
useDebugLogging = False

initializeContext :: IO A.Env
initializeContext = initEnv
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

standaloneHandler :: Value -> Aws.Context A.Env
                  -> IO (Either String Text)
standaloneHandler req awsContext = do
  env <- readIORef $ customContext awsContext

  case parseQueryParams req of
    Error errStr -> pure $ Left errStr
    Success (QueryParams uuid) -> do
      alarms <- getAlarmsByUUID env "test-table" uuid
      pure $ Right (text uuid alarms)
  where
    text :: UUID -> [Item] -> Text
    text uuid alarms = ""
      <> "uuid value: " <> uuid <> "\n"
      <> "existing alarms: " <> T.pack (show alarms)

-- | aws dynamodb query --table-name test-table --key-condition-expression "#u = :u" --expression-attribute-names '{"#u":"uuid"}' --expression-attribute-values '{":u":{"S":"abcd1234"}}'
getAlarmsByUUID :: A.Env -> TableName -> UUID -> IO [Item]
getAlarmsByUUID env tableName uuid = do
  A.runResourceT $ do
    runConduit $
      A.paginate env query
        .| CL.map D.items
        .| CL.concat
        .| CC.sinkList
  where
    query =
      (D.newQuery tableName)
      { D.expressionAttributeNames = Just (HM.fromList [ ("#u", "uuid") ])
      , D.keyConditionExpression = Just "#u = :u"
      , D.expressionAttributeValues = Just (HM.fromList [ (":u", DA.S uuid) ])
      }

parseQueryParams :: Value -> Result QueryParams
parseQueryParams (Object rootObj) =
  case KeyMap.lookup "queryStringParameters" rootObj of
    Just v@(Object _) -> fromJSON v
    _ -> Error "queryStringParameters field not found or not an Object"
parseQueryParams _ = Error "root JSON Value is not an Object"
