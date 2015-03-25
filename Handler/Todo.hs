module Handler.Todo where

import Import hiding (lookup)

import Model.Todo

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as B
import Network.Wai (strictRequestBody)
import Yesod.Auth

getTodoR :: Handler Html
getTodoR = do
  defaultLayout $ do
    addScript $ StaticR ghcjs_todo_js
    $(widgetFile "todo")

getTodosR :: Handler TypedContent
getTodosR = do
  ts <- runDB fetchTodos
  print ts
  let r = object [ "todos" .= ts ]
  -- liftIO $ B.putStrLn $ encode $ r
  selectRep $ provideRep $ return r

postTodosR :: Handler TypedContent
postTodosR = do
  i <- waiRequest
  j <- liftIO $ strictRequestBody i
  case decode j :: Maybe Todo of
    Just t -> do
      tid <- runDB $ insert t
      selectRep $ do
        provideRep $ return $ toJSON $ Entity tid t
    Nothing -> invalidArgs ["post"]

deleteTodosNR :: Int64 -> Handler ()
deleteTodosNR  n = runDB $ delete k
  where
    k :: Key Todo
    Right k = keyFromValues [PersistInt64 n]

putTodosNR :: Int64 -> Handler ()
putTodosNR  n = do
  i <- waiRequest
  j <- liftIO $ strictRequestBody i
  liftIO $ print j
  case decode j :: Maybe Todo of
    Just t -> do
      runDB $ replace k t
    Nothing -> invalidArgs ["post"]
  where
    k :: Key Todo
    Right k = keyFromValues [PersistInt64 n]
