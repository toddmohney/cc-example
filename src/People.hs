module People
( findOrCreatePerson
, selectAllPeople
) where

import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Text (Text)
import           Database.Persist.Sql
import           Database.Persist.Sqlite as DB
import qualified Models as M

selectAllPeople :: ConnectionPool -> IO [Entity M.Person]
selectAllPeople pool =
  let query = DB.selectList ([] :: [DB.Filter M.Person]) []
  in runStderrLoggingT $ runSqlPool query pool

selectPerson :: ConnectionPool -> M.Person -> IO (Maybe (Entity M.Person))
selectPerson pool p =
  let query = DB.selectFirst [M.PersonName ==. M.personName p] []
  in runStderrLoggingT $ runSqlPool query pool

findOrCreatePerson :: ConnectionPool -> M.Person -> IO M.PersonId
findOrCreatePerson pool person = selectPerson pool person >>= \existingPerson ->
  case existingPerson of
    Nothing  -> runStderrLoggingT $ runSqlPool (DB.insert person) pool
    (Just p) -> return . DB.entityKey $ p
