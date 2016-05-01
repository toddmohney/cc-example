{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module MeatbarApi
( MeatbarApi
, meatbarApi
, meatbarServer
) where

import           Api.Types.Meatbars.EatenBar (EatenBar, toEatenBar)
import           App
import           AppConfig
import           Control.Monad.Reader
import qualified Meatbars as MB
import           PeopleApi (PeopleApi, peopleServer)
import           Servant

type MeatbarApi = PeopleApi
             :<|> MeatbarsApi

type MeatbarsApi = "meatbars" :> "consumption" :> Get '[JSON] [EatenBar]

meatbarApi :: Proxy MeatbarApi
meatbarApi = Proxy

meatbarServer :: ServerT MeatbarApi App
meatbarServer = peopleServer
           :<|> getEatenMeatbars

getEatenMeatbars :: App [EatenBar]
getEatenMeatbars =  reader getDBPool >>= \dbPool ->
  (liftIO $ MB.selectAllEatenMeatbars dbPool) >>= \eatenBarModels ->
  return $ map toEatenBar eatenBarModels
