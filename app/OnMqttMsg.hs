{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aws.Lambda as Lambda
import qualified Amazonka as A
import qualified Amazonka.DynamoDB as D
import qualified Amazonka.DynamoDB.PutItem as D
import qualified Amazonka.DynamoDB.Types.AttributeValue as DA
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Data.IORef
import System.IO (stdout)
import Data.Word (Word64)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.HashMap.Strict as HM


type TableName = Text

type ID = Text

newtype Data = Data Word64
  deriving Show

type SortKey = Word64

data Msg = Msg ID Data
  deriving Show

instance FromJSON Msg where
  parseJSON (Object v) = Msg <$> v .: "id" <*> v .: "nested"
  parseJSON _ = mempty

instance FromJSON Data where
  parseJSON (Object v) = Data <$> v.: "value"
  parseJSON _ = mempty

-- | Toggle debug logging here.
useDebugLogging :: Bool
useDebugLogging = False

-- | Set table name here, where to update/put items.
theOnlyTableName :: Text
theOnlyTableName = "test-table"

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

standaloneHandler :: Value -> Lambda.Context A.Env
                  -> IO (Either String Text)
standaloneHandler req awsContext = do
  env <- readIORef $ customContext awsContext
  -- get output (Either values) using `aws lambda invoke`
  case fromJSON req :: Result Msg of
    Error str -> pure $ Left str
    Success msg -> do
      putItemResp <- doPutItem env theOnlyTableName msg
      pure $ Right $ T.pack (show req) <> "\n"
        <> T.pack (show msg) <> "\n" <> T.pack (show putItemResp)

genSortKeyValue :: IO Word64
genSortKeyValue = round . (* 10^(9 :: Word64)) <$> getPOSIXTime

doPutItem :: A.Env -> TableName -> Msg -> IO D.PutItemResponse
doPutItem env tableName (Msg id (Data number)) = do
  sortKey <- T.pack . show <$> genSortKeyValue
  let itemMap = HM.fromList
                [ ("id", DA.S id)
                , ("timestamp", DA.N sortKey)
                , ("value", DA.N $ T.pack $ show number)
                ]
  A.runResourceT
    $ A.send env
    $ setPutItem (D.newPutItem tableName) itemMap
  where
    setPutItem newItem itemMap = newItem { D.item = itemMap }
