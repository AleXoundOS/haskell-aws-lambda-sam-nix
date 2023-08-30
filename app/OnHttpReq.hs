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

type ID = Text

newtype QueryParams = QueryParams ID
  deriving Show

instance FromJSON QueryParams where
  parseJSON (Object o) = QueryParams <$> o .: "id"
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
    Success (QueryParams id) -> do
      items <- getItemsByID env "test-table" id
      pure $ Right (text id items)
  where
    text :: ID -> [Item] -> Text
    text id items = ""
      <> "id value: " <> id <> "\n"
      <> "existing items: " <> T.pack (show items)

-- | aws dynamodb query --table-name test-table --key-condition-expression "#u = :u" --expression-attribute-names '{"#u":"id"}' --expression-attribute-values '{":u":{"S":"abcd1234"}}'
getItemsByID :: A.Env -> TableName -> ID -> IO [Item]
getItemsByID env tableName id = do
  A.runResourceT $ do
    runConduit $
      A.paginate env query
        .| CL.map D.items
        .| CL.concat
        .| CC.sinkList
  where
    query =
      (D.newQuery tableName)
      { D.expressionAttributeNames = Just (HM.fromList [ ("#i", "id") ])
      , D.expressionAttributeValues = Just (HM.fromList [ (":i", DA.S id) ])
      , D.keyConditionExpression = Just "#i = :i"
      }

parseQueryParams :: Value -> Result QueryParams
parseQueryParams (Object rootObj) =
  case KeyMap.lookup "queryStringParameters" rootObj of
    Just v@(Object _) -> fromJSON v
    _ -> Error "queryStringParameters field not found or not an Object"
parseQueryParams _ = Error "root JSON Value is not an Object"
