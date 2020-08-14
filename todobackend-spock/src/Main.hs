{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class (liftIO)
import qualified Database.Persist.Sqlite as Sqlite
import Network.HTTP.Types.Status (status404)
import System.Environment
import Web.Spock.Core
import Web.PathPieces

import TodoBackend.Model
import TodoBackend.Utils

json' :: String -> Sqlite.Entity Todo -> ActionT IO ()
json' url = json . mkTodoResponse url

jsonList :: String -> [Sqlite.Entity Todo] -> ActionT IO ()
jsonList url = json . map (mkTodoResponse url)

main :: IO ()
main = do
  runDb $ Sqlite.runMigration migrateAll
  port <- read <$> getEnv "PORT"
  url <- getEnv "URL"
  runSpock port $ spockT id $ do
    middleware allowCors
    middleware allowOptions
    get (sub root) $ do
        todos <- liftIO $ runDb $ Sqlite.selectList [] ([] :: [Sqlite.SelectOpt Todo])
        jsonList url todos
    get (sub var) $ \tid -> actionOr404 tid (\ident -> do
                    mtodo <- liftIO $ runDb $ Sqlite.get
                                  (ident :: TodoId)
                    case mtodo of
                      Nothing -> setStatus status404
                      Just todo -> json' url (Sqlite.Entity ident todo))
    patch (sub var) $ \tid -> actionOr404 tid (\ident -> do
                        todoAct <- jsonBody'
                        let todoUp = actionToUpdates todoAct
                        todo <- liftIO $ runDb $ Sqlite.updateGet
                                ident todoUp
                        json' url (Sqlite.Entity ident todo))
    delete (sub var) $ \tid -> actionOr404 tid (\ident ->
                          liftIO $ runDb $ Sqlite.delete (ident :: TodoId))
    post (sub root) $ do
        todoAct <- jsonBody'
        let todo = actionToTodo todoAct
        tid <- liftIO $ runDb $ Sqlite.insert todo
        json' url (Sqlite.Entity tid todo)
    delete (sub root) $ liftIO $ runDb $ Sqlite.deleteWhere ([] :: [Sqlite.Filter Todo])
  where
    actionOr404 pid action = case fromPathPiece pid of
            Nothing  -> setStatus status404
            Just tid -> action tid
    sub = (<//>)"todos"
