module People
( findOrCreatePerson
, selectAllPeople
) where

import           Control.Monad.Logger (runStderrLoggingT)
import           Control.Monad.Reader
import           Data.Text (Text)
import           Database
import           Database.Persist.Sql
import           Database.Persist.Sqlite as DB
import qualified Models as M

selectAllPeople :: WithDB [Entity M.Person]
selectAllPeople = withDBConn $ DB.selectList ([] :: [DB.Filter M.Person]) []

selectPerson :: M.Person -> WithDB (Maybe (Entity M.Person))
selectPerson p = withDBConn $ DB.selectFirst [M.PersonName ==. M.personName p] []

findOrCreatePerson :: M.Person -> WithDB M.PersonId
findOrCreatePerson person = selectPerson person >>= \existingPerson ->
  case existingPerson of
    Nothing  -> withDBConn $ DB.insert person
    (Just p) -> return . DB.entityKey $ p
