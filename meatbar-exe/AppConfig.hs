{-# LANGUAGE OverloadedStrings #-}

module AppConfig
( AppConfig (..)
, getAppConfig
) where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sql
import Database.Persist.Sqlite
import Models (migrateAll)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai                          (Middleware)

data AppConfig =
  AppConfig { getRequestLogger :: Middleware
            , getDBPool :: ConnectionPool
            }

getAppConfig :: IO AppConfig
getAppConfig = dbPool >>= \pool ->
  return $ AppConfig logStdoutDev pool

dbPool :: IO ConnectionPool
dbPool = do
  pool <- runStderrLoggingT $ do
    p  <- createSqlitePool "dev.meatbar.db" 1
    runSqlPool (runMigration migrateAll) p
    return p
  return pool
