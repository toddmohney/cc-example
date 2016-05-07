{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database
( WithDB (..)
, withDBConn
) where

import Control.Monad.Logger
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
import Database.Persist.Sql

newtype WithDB a = WithDB { runDB :: ReaderT ConnectionPool IO a }
  deriving (Functor, Applicative, Monad, MonadReader ConnectionPool, MonadIO)

withDBConn :: SqlPersistT (LoggingT IO) a -> WithDB a
withDBConn query =
  ask >>= liftIO . runStderrLoggingT . (runSqlPool query)
