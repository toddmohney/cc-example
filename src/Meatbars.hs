module Meatbars
( createMeatbar
) where

import           Control.Monad.Logger (runStderrLoggingT)
import           Database.Persist.Sql
import           Database.Persist.Sqlite as DB
import qualified Models as M

createMeatbar :: ConnectionPool -> M.Meatbar -> IO M.MeatbarId
createMeatbar pool meatbar =
  runStderrLoggingT $ runSqlPool (DB.insert meatbar) pool
