{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aws.Lambda as Aws
import qualified Amazonka as A
import qualified Amazonka.DynamoDB as D
import qualified Amazonka.DynamoDB.UpdateItem as D
import qualified Amazonka.DynamoDB.Types.AttributeValue as DA
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types (parseFail, parse)
import Data.IORef
import Data.Vector as V ((!?))
import System.IO (stdout)
import Control.Monad ((<=<))
import qualified Data.HashMap.Strict as HM


type Filename = Text

-- | Toggle debug logging here.
useDebugLogging :: Bool
useDebugLogging = False

-- | Set table name here, where to update/put items.
tableName :: Text
tableName = "test-table"

-- | This should be run only on startup of each instance of a Lambda function.
initializeContext :: IO A.Env
initializeContext =
  if useDebugLogging then do
    logger <- A.newLogger A.Debug stdout
    setLoggerInAwsEnv logger <$> A.newEnv A.discover
  else
    A.newEnv A.discover
  where
    setLoggerInAwsEnv :: A.Logger -> A.Env -> A.Env
    setLoggerInAwsEnv logger env = env { A.logger = logger }


main :: IO ()
main = do
  runLambdaHaskellRuntime defaultDispatcherOptions initializeContext id $ do
    addStandaloneLambdaHandler "standaloneHandler" standaloneHandler

standaloneHandler :: Value -> Aws.Context A.Env -> IO (Either String Text)
standaloneHandler req awsContext = do
  case parseFilenameFromAwsJSON req of
    Error str -> print str
    Success filename -> do
      env <- readIORef $ customContext awsContext
      print =<< doUpdateItem env "abcd1234" "1693213852670264533" filename
  pure $ Right "handler has finished"

-- | Update (or create) a new item with specific key and filename attribute.
doUpdateItem :: A.Env -> Text -> Text -> Text
             -> IO D.UpdateItemResponse
doUpdateItem env uuid sortKey filename = A.runResourceT
  $ A.send env
  $ (D.newUpdateItem tableName)
  { D.expressionAttributeNames = Just (HM.fromList [ ("#F", "filename") ])
  , D.expressionAttributeValues = Just (HM.fromList [ (":f", DA.S filename) ])
  , D.key = HM.fromList
            [ ("uuid", DA.S uuid)
            , ("timestamp", DA.N sortKey)
            ]
  , D.updateExpression = Just "SET #F = :f"
  }

parseFilenameFromAwsJSON :: Value -> Result Filename
parseFilenameFromAwsJSON = parse $ pure
  <=< withObject "key Text" (.: "key")
  <=< withObject "object Object" (.: "object")
  <=< withObject "S3 Object" (.: "s3")
  <=< withArray "Records Array" (maybe (parseFail "no elements") pure . (!? 0))
  <=< withObject "AWS input" (.: "Records")
