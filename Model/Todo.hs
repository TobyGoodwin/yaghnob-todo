module Model.Todo where

import Import

-- fetchTodos :: (MonadResource m, MonadSqlPersist m) => m [Entity Todo]
fetchTodos :: MonadIO m => ReaderT SqlBackend m [Entity Todo]
fetchTodos = do
  ts <- selectList [] [Asc TodoId]
  return ts
