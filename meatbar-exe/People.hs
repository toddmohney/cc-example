{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module People
( PeopleApi
, Person (..)
, peopleApi
, peopleServer
) where

import           Api.Types (Person (..))
import           App (App)
import           AppConfig
import           Control.Monad.Logger (runStderrLoggingT)
import           Control.Monad.Reader
import           Database.Persist.Sqlite as DB
import qualified Models as M
import           Servant

type PeopleApi = "people" :> Get '[JSON] [Person]

peopleApi :: Proxy PeopleApi
peopleApi = Proxy

peopleServer :: ServerT PeopleApi App
peopleServer = getPeople

getPeople :: App [Person]
getPeople = selectAllPeople >>= \peopleModels ->
  return $ map toPerson peopleModels

selectAllPeople :: App [Entity M.Person]
selectAllPeople = ask >>= \cfg ->
  let query = DB.selectList ([] :: [DB.Filter M.Person]) []
  in liftIO $ runStderrLoggingT $ runSqlPool query (getDBPool cfg)

toPerson :: Entity M.Person -> Person
toPerson p =
  let person   = DB.entityVal p
      personId = DB.entityKey p
  in Person personId (M.personName person)

