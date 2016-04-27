{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module People
( PeopleApi
, peopleApi
, peopleServer
) where

import App (App)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant

data Person = Person { name :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON Person
instance FromJSON Person

type PeopleApi = "people" :> Get '[JSON] [Person]

peopleApi :: Proxy PeopleApi
peopleApi = Proxy

peopleServer :: ServerT PeopleApi App
peopleServer = getPeople

getPeople :: App [Person]
getPeople = undefined
