{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Meatbar
( MeatbarApi
, meatbarApi
, meatbarServer
) where

import App
import People (PeopleApi, peopleServer)
import Servant

type MeatbarApi = PeopleApi

meatbarApi :: Proxy MeatbarApi
meatbarApi = Proxy

meatbarServer :: ServerT MeatbarApi App
meatbarServer = peopleServer
