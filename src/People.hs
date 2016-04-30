module People
( createPerson
, selectAllPeople
) where

import           Control.Monad.Logger (runStderrLoggingT)
import           Database.Persist.Sql
import           Database.Persist.Sqlite as DB
import qualified Models as M

selectAllPeople :: ConnectionPool -> IO [Entity M.Person]
selectAllPeople pool =
  let query = DB.selectList ([] :: [DB.Filter M.Person]) []
  in runStderrLoggingT $ runSqlPool query pool

createPerson :: ConnectionPool -> M.Person -> IO M.PersonId
createPerson pool person =
  runStderrLoggingT $ runSqlPool (DB.insert person) pool
