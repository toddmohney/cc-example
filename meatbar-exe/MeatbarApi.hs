{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module MeatbarApi
( MeatbarApi
, meatbarApi
, meatbarServer
) where

import App
import PeopleApi (PeopleApi, peopleServer)
import Servant

type MeatbarApi = PeopleApi

meatbarApi :: Proxy MeatbarApi
meatbarApi = Proxy

meatbarServer :: ServerT MeatbarApi App
meatbarServer = peopleServer
