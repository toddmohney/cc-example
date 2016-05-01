module Meatbars
( createMeatbar
, createEatenBar
, selectAllMeatbars
) where

import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Text (Text)
import           Database.Persist.Sql
import           Database.Persist.Sqlite as DB
import qualified Models as M

selectAllMeatbars :: ConnectionPool -> IO [Entity M.Meatbar]
selectAllMeatbars pool =
  let query = DB.selectList ([] :: [DB.Filter M.Meatbar]) []
  in runStderrLoggingT $ runSqlPool query pool

createMeatbar :: ConnectionPool -> M.Meatbar -> IO M.MeatbarId
createMeatbar pool meatbar =
  runStderrLoggingT $ runSqlPool (DB.insert meatbar) pool

createEatenBar :: ConnectionPool -> M.EatenBar -> IO M.EatenBarId
createEatenBar pool eatenBar =
  runStderrLoggingT $ runSqlPool (DB.insert eatenBar) pool
