{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
import qualified Control.Category as C
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Reader         (ReaderT, runReaderT, asks)
import           Control.Monad.Trans.Except
import           Data.Proxy
import qualified Database.Persist.Sqlite      as Sqlite
import           GHC.TypeLits (Nat)
import           Network.Wai
import           Network.Wai.Handler.Warp     (run)
import           Servant
import           Servant.Server (hoistServer, err404)
import           System.Environment

import           TodoBackend.Model
import           TodoBackend.Utils


data App = App
  {
    appRoot :: String
  }

type TodoApi = "todos" :> Get '[JSON] [TodoResponse]
            :<|> "todos" :> Delete '[JSON] ()
            :<|> "todos" :> ReqBody '[JSON] TodoAction :> PostCreated '[JSON] TodoResponse
            :<|> "todos" :> Capture "todoid" Integer :> Get '[JSON] TodoResponse
            :<|> "todos" :> Capture "todoid" Integer :> Delete '[JSON] ()
            :<|> "todos" :> Capture "todoid" Integer :> ReqBody '[JSON] TodoAction :> Patch '[JSON] TodoResponse

type AppM = ReaderT App Handler

toResp :: Sqlite.Entity Todo -> AppM TodoResponse
toResp todo = do
  url <- asks appRoot
  return $ mkTodoResponse url todo

toRespL :: [Sqlite.Entity Todo] -> AppM [TodoResponse]
toRespL todos = do
  url <- asks appRoot
  return $ map (mkTodoResponse url) todos

todoApi :: Proxy TodoApi
todoApi = Proxy

getTodos :: AppM [TodoResponse]
getTodos = do
  todos <- liftIO $ runDb $ Sqlite.selectList [] ([] :: [Sqlite.SelectOpt Todo])
  toRespL todos

deleteTodos :: AppM ()
deleteTodos =  liftIO $ runDb $ Sqlite.deleteWhere ([] :: [Sqlite.Filter Todo])

getTodo :: Integer -> AppM TodoResponse
getTodo tid = do
  let tKey = Sqlite.toSqlKey (fromIntegral tid)
  mtodo <- liftIO $ runDb $ Sqlite.get tKey
  case mtodo of
    Nothing -> throwError err404
    Just todo -> toResp $ Sqlite.Entity tKey todo

deleteTodo :: Integer -> AppM ()
deleteTodo tid = do
  let tKey = Sqlite.toSqlKey (fromIntegral tid)
  liftIO $ runDb $ Sqlite.delete (tKey :: Sqlite.Key Todo)

postTodo :: TodoAction -> AppM TodoResponse
postTodo todoAct = do
  let todo = actionToTodo todoAct
  tid <- liftIO $ runDb $ Sqlite.insert todo
  toResp $ Sqlite.Entity tid todo

patchTodo :: Integer -> TodoAction -> AppM TodoResponse
patchTodo tid todoAct = do
  let tKey = Sqlite.toSqlKey (fromIntegral tid)
      updates = actionToUpdates todoAct
  todo <- liftIO $ runDb $ Sqlite.updateGet tKey updates
  toResp $ Sqlite.Entity tKey todo

server :: ServerT TodoApi AppM
server =      getTodos
         :<|> deleteTodos
         :<|> postTodo
         :<|> getTodo
         :<|> deleteTodo
         :<|> patchTodo

readerServer :: App -> Server TodoApi
readerServer app = hoistServer todoApi nt server
  where
    nt :: AppM x -> Handler x
    nt = flip runReaderT app

waiApp :: App -> Application
waiApp app = allowCors $ allowOptions $ serve todoApi (readerServer app)

main :: IO ()
main = do
  runDb $ Sqlite.runMigration migrateAll
  port <- read <$> getEnv "PORT"
  url <- getEnv "URL"
  run port $ waiApp (App url)
