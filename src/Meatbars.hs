module Meatbars
( EatenMeatbar (..)
, createMeatbar
, createEatenBar
, selectAllMeatbars
, selectAllEatenMeatbars
) where

import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import           Database.Persist.Sql
import           Database.Persist.Sqlite as DB
import qualified Models as M

data EatenMeatbar =
  EatenMeatbar { getPerson :: Entity M.Person
               , getMeatbar :: Entity M.Meatbar
               , getEatenBar :: Entity M.EatenBar
               }

selectAllMeatbars :: ConnectionPool -> IO [Entity M.Meatbar]
selectAllMeatbars pool =
  let query = DB.selectList ([] :: [DB.Filter M.Meatbar]) []
  in runStderrLoggingT $ runSqlPool query pool

selectAllEatenMeatbars :: ConnectionPool -> IO [EatenMeatbar]
selectAllEatenMeatbars pool =
  (runStderrLoggingT $ runSqlPool query pool) >>= (return . (fmap toEatenMeatbar))
  where
    query =
      E.select $ E.from $ \(eatenBar `E.InnerJoin` person `E.InnerJoin` meatbar) -> do
        E.on $ eatenBar ^. M.EatenBarPersonId E.==. person ^. M.PersonId
        E.on $ eatenBar ^. M.EatenBarMeatbarId E.==. meatbar ^. M.MeatbarId
        return (eatenBar, person, meatbar)

createMeatbar :: ConnectionPool -> M.Meatbar -> IO M.MeatbarId
createMeatbar pool meatbar =
  runStderrLoggingT $ runSqlPool (DB.insert meatbar) pool

createEatenBar :: ConnectionPool -> M.EatenBar -> IO M.EatenBarId
createEatenBar pool eatenBar =
  runStderrLoggingT $ runSqlPool (DB.insert eatenBar) pool

toEatenMeatbar :: (E.Entity M.EatenBar, E.Entity M.Person, E.Entity M.Meatbar) -> EatenMeatbar
toEatenMeatbar (eatenBar, person, meatbar) =
  EatenMeatbar person meatbar eatenBar
