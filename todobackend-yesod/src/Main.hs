{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
import qualified Database.Persist.Sqlite as Sqlite
import           Network.Wai.Handler.Warp     (run)
import           System.Environment
import           Yesod

import           TodoBackend.Model
import           TodoBackend.Utils


data App = App

mkYesod "App" [parseRoutes|
/todos         TodosR GET POST  DELETE
/todos/#TodoId TodoR  GET PATCH DELETE
|]

instance Yesod App

getTodosR :: Handler Value
getTodosR = do
  todos <- liftIO $ runDb $ Sqlite.selectList [] ([] :: [Sqlite.SelectOpt Todo])
  returnJson todos

postTodosR :: Handler Value
postTodosR = do
  todoAct <- requireJsonBody
  let todo = actionToTodo todoAct
  tid <- liftIO $ runDb $ Sqlite.insert todo
  returnJson $ Sqlite.Entity tid todo

deleteTodosR :: Handler ()
deleteTodosR = do
  liftIO $ runDb $ Sqlite.deleteWhere ( [] :: [Sqlite.Filter Todo])
  return ()

getTodoR :: TodoId -> Handler Value
getTodoR tid = do
    todo <- liftIO $ runDb $ get404 tid
    returnJson $ Sqlite.Entity tid todo

patchTodoR :: TodoId -> Handler Value
patchTodoR tid = do
  todoAct <- requireJsonBody
  let todoUp = actionToUpdates todoAct
  todo <- liftIO $ runDb $ Sqlite.updateGet tid todoUp
  returnJson $ Sqlite.Entity tid todo

deleteTodoR :: TodoId -> Handler ()
deleteTodoR tid = do
  liftIO $ runDb $ Sqlite.delete tid
  return ()

mkApp :: Application -> Application
mkApp a = allowCors $ allowOptions a

main :: IO ()
main = do
  runDb $ Sqlite.runMigration migrateAll
  port <- read <$> getEnv "PORT"
  waiApp <- toWaiApp App
  run port $ mkApp waiApp
