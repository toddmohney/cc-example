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

import           Api.Types.People (Person (..), toPerson)
import           App (App)
import           AppConfig
import           Control.Monad.Reader
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

