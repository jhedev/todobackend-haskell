{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Database.Persist.Sqlite as Sqlite
import Snap
import Snap.Extras.JSON
import System.Environment

import TodoBackend.Model

site :: Snap ()
site =
    route [ ("todos", optionsResp)
          , ("todos", todosHandler)
          , ("todos/:todoid", optionsResp)
          , ("todos/:todoid", todoHandler)
          ]

allowCors :: Snap ()
allowCors = mapM_ (modifyResponse . uncurry setHeader) [
    ("Access-Control-Allow-Origin", "*"),
    ("Access-Control-Allow-Headers", "Accept, Content-Type"),
    ("Access-Control-Allow-Methods", "GET, HEAD, POST, DELETE, OPTIONS, PUT, PATCH")
  ]

optionsResp :: Snap ()
optionsResp = method OPTIONS allowCors

todosHandler :: Snap ()
todosHandler = do
  allowCors
  req <- getRequest
  case rqMethod req of
   GET -> do
      todos <- liftIO $ runDb $
        Sqlite.selectList [] ([] :: [Sqlite.SelectOpt Todo])
      writeJSON todos
   POST -> do
      todoActE <- getJSON
      case todoActE of
        Right todoAct -> do
            let todo = actionToTodo todoAct
            tid <- liftIO $ runDb $ Sqlite.insert todo
            writeJSON $ Sqlite.Entity tid todo
        Left _ -> writeBS "error"
   DELETE -> liftIO $ runDb $ Sqlite.deleteWhere ([] :: [Sqlite.Filter Todo])
   _ -> writeBS "error"

todoHandler :: Snap ()
todoHandler = do
  allowCors
  Just tidBS <- getParam "todoid"
  req <- getRequest
  case C8.readInteger tidBS of
    Nothing       -> writeBS "error"
    Just (n, _) -> do
        let tid = Sqlite.toSqlKey $ fromIntegral n
        case rqMethod req of
            GET -> do
                Just todo <- liftIO $ runDb $ Sqlite.get tid
                writeJSON $ Sqlite.Entity tid todo
            PATCH -> do
                todoActE <- getJSON
                case todoActE of
                  Left _        -> writeBS "error"
                  Right todoAct -> do
                    let todoUp = actionToUpdates todoAct
                    todo <- liftIO $ runDb $ Sqlite.updateGet tid todoUp
                    writeJSON $ Sqlite.Entity tid todo
            DELETE -> liftIO $ runDb $ Sqlite.delete tid
            _ -> undefined

main :: IO ()
main = do
  runDb $ Sqlite.runMigration migrateAll
  port <- read <$> getEnv "PORT"
  let config = setPort port defaultConfig
  httpServe config site
