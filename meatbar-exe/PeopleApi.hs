{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module PeopleApi
( PeopleApi
, Person (..)
, peopleApi
, peopleServer
) where

import           Api.Types (Person (..))
import           App (App)
import           AppConfig
import           Control.Monad.Reader
import           Database.Persist.Sqlite as DB
import qualified Models as M
import qualified People as P
import           Servant

type PeopleApi = "people" :> Get '[JSON] [Person]

peopleApi :: Proxy PeopleApi
peopleApi = Proxy

peopleServer :: ServerT PeopleApi App
peopleServer = getPeople

getPeople :: App [Person]
getPeople = reader getDBPool >>= \dbPool ->
  (liftIO $ P.selectAllPeople dbPool) >>= \peopleModels ->
  return $ map toPerson peopleModels

toPerson :: Entity M.Person -> Person
toPerson p =
  let person   = DB.entityVal p
      personId = DB.entityKey p
  in Person personId (M.personName person)

