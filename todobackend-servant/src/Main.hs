{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
import           Control.Applicative          ((<$>))
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Either
import           Data.Proxy
import qualified Database.Persist.Sqlite      as Sqlite
import           Network.Wai
import           Network.Wai.Handler.Warp     (run)
import           Servant
import           System.Environment

import           TodoBackend.Model
import           TodoBackend.Utils


type TodoApi = "todos" :> Get '[JSON] [Sqlite.Entity Todo]
            :<|> "todos" :> Delete '[JSON] ()
            :<|> "todos" :> ReqBody '[JSON] TodoAction :> Post '[JSON] (Sqlite.Entity Todo)
            :<|> "todos" :> Capture "todoid" Integer :> Get '[JSON] (Sqlite.Entity Todo)
            :<|> "todos" :> Capture "todoid" Integer :> Delete '[JSON] ()
            :<|> "todos" :> Capture "todoid" Integer :> ReqBody '[JSON] TodoAction :> Patch '[JSON] (Sqlite.Entity Todo)

todoApi :: Proxy TodoApi
todoApi = Proxy

getTodos :: EitherT ServantErr IO [Sqlite.Entity Todo]
getTodos = liftIO $ runDb $ Sqlite.selectList [] ([] :: [Sqlite.SelectOpt Todo])

deleteTodos :: EitherT ServantErr IO ()
deleteTodos =  liftIO $ runDb $ Sqlite.deleteWhere ([] :: [Sqlite.Filter Todo])

getTodo :: Integer -> EitherT ServantErr IO (Sqlite.Entity Todo)
getTodo tid = do
  let tKey = Sqlite.toSqlKey (fromIntegral tid)
  Just todo <- liftIO $ runDb $ Sqlite.get tKey
  return $ Sqlite.Entity tKey todo

deleteTodo :: Integer -> EitherT ServantErr IO ()
deleteTodo tid = do
  let tKey = Sqlite.toSqlKey (fromIntegral tid)
  liftIO $ runDb $ Sqlite.delete (tKey :: Sqlite.Key Todo)

postTodo :: TodoAction -> EitherT ServantErr IO (Sqlite.Entity Todo)
postTodo todoAct = do
  let todo = actionToTodo todoAct
  tid <- liftIO $ runDb $ Sqlite.insert todo
  return $ Sqlite.Entity tid todo

patchTodo :: Integer -> TodoAction -> EitherT ServantErr IO (Sqlite.Entity Todo)
patchTodo tid todoAct = do
  let tKey = Sqlite.toSqlKey (fromIntegral tid)
      updates = actionToUpdates todoAct
  todo <- liftIO $ runDb $ Sqlite.updateGet tKey updates
  return $ Sqlite.Entity tKey todo

server :: Server TodoApi
server =      getTodos
         :<|> deleteTodos
         :<|> postTodo
         :<|> getTodo
         :<|> deleteTodo
         :<|> patchTodo

waiApp :: Application
waiApp = allowCors $ allowOptions $ serve todoApi server

main :: IO ()
main = do
  runDb $ Sqlite.runMigration migrateAll
  port <- read <$> getEnv "PORT"
  run port waiApp
