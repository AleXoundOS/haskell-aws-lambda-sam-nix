{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aws.Lambda as Aws
import qualified Amazonka as A
import qualified Amazonka.DynamoDB as D
import qualified Amazonka.DynamoDB.UpdateItem as DU
import qualified Amazonka.DynamoDB.Query as DQ
import qualified Amazonka.DynamoDB.Types.AttributeValue as DA
import Data.Text (Text)
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.Aeson
import Data.Aeson.Types (parseFail, parseEither)
import Data.IORef
import Data.Vector as V ((!?))
import System.IO (stdout)
import Control.Monad ((<=<))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)


type Filename = Text
type Filepath = Text
type Folder = Text
type TableName = Text

-- | Toggle debug logging here.
useDebugLogging :: Bool
useDebugLogging = False

-- | Set table name here, where to update/put items.
theOnlyTableName :: Text
theOnlyTableName = "test-table"

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
standaloneHandler req awsContext =
  -- parse (from JSON) folder and filename of just uploaded file to S3 bucket
  case parseFilenameAndFolderFromJSON req of
    Left str -> pure $ Left str
    Right (folder, filename) -> do
      -- get AWS environment (required to operate with DynamoDB)
      env <- readIORef $ customContext awsContext
      -- query timestamps (by Local Secondary Index) in specific value range
      timestamps <- getTsInValRange env theOnlyTableName partKey range
      -- update filename attribute in each item (from the query above)
      mapM_ (doUpdateItem env theOnlyTableName filename partKey) timestamps
      -- TODO check database response and return Left is there is fail
      pure $ Right "handler has finished"
      where
        -- assume that folder name corresponds to the table partition key
        partKey = folder
        -- assume that filename is always just a number
        number = read $ T.unpack filename :: Int
        range = (number, number + 1000)

getTsInValRange :: A.Env -> TableName -> Text -> (Int, Int) -> IO [Text]
getTsInValRange env tableName partKey (v1, v2) =
  A.runResourceT $ do
    runConduit $
      A.paginate env query
        .| CL.map tsValFromItem -- [[Maybe Text]]
        .| CL.concat -- [Maybe Text]
        .| CL.catMaybes -- [Text]
        .| CC.sinkList
  where
    query =
      (DQ.newQuery tableName)
      { DQ.indexName = Just "value-index" -- use LSI
      , DQ.expressionAttributeNames = Just (HM.fromList
                                           [ ("#pk", "id")
                                           , ("#v", "value")
                                           , ("#t", "timestamp")
                                           ])
      , DQ.expressionAttributeValues =
          Just (HM.fromList
                [ (":i", DA.S partKey)
                , (":v1", DA.N $ T.pack $ show v1)
                , (":v2", DA.N $ T.pack $ show v2)
                ])
      , DQ.keyConditionExpression = Just "#pk = :i AND #v BETWEEN :v1 AND :v2"
      , DQ.projectionExpression = Just "#t"
      }
    tsValFromItem :: DQ.QueryResponse -> [Maybe Text]
    tsValFromItem = map (parseN <=< HM.lookup "timestamp") . DQ.items
    parseN (DA.N t) = Just t
    parseN _ = Nothing

-- | Update (or create) a new item with specific key and filename attribute.
doUpdateItem :: A.Env -> TableName -> Text -> Text -> Text
             -> IO D.UpdateItemResponse
doUpdateItem env tableName filename partKey sortKey = A.runResourceT
  $ A.send env
  $ (DU.newUpdateItem tableName)
  { DU.expressionAttributeNames = Just (HM.fromList [ ("#F", "filename") ])
  , DU.expressionAttributeValues = Just (HM.fromList [ (":f", DA.S filename) ])
  , DU.key = HM.fromList
            [ ("id", DA.S partKey)
            , ("timestamp", DA.N sortKey)
            ]
  , DU.updateExpression = Just "SET #F = :f"
  }

parseFilenameAndFolderFromJSON :: Value -> Either String (Folder, Filename)
parseFilenameAndFolderFromJSON v =
  parseFilepath v >>= parseFilenameAndFolder

parseFilenameAndFolder :: Filepath -> Either String (Folder, Filename)
parseFilenameAndFolder filepath =
  case T.split (== '/') filepath of
    [folder, filename] -> Right (folder, filename)
    _ -> Left "unexpected file path hierarchy"

parseFilepath :: Value -> Either String Filepath
parseFilepath = parseEither $ pure
  <=< withObject "key Text" (.: "key")
  <=< withObject "object Object" (.: "object")
  <=< withObject "S3 Object" (.: "s3")
  <=< withArray "Records Array" (maybe (parseFail "no elements") pure . (!? 0))
  <=< withObject "AWS input" (.: "Records")
