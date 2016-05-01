{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module MeatbarApi
( MeatbarApi
, meatbarApi
, meatbarServer
) where

import           Api.Types.Meatbars.EatenBar (EatenBar, toEatenBar)
import           Api.Types.Meatbars.EatStreak (EatStreak (..), toEatStreaks)
import           App
import           AppConfig
import           Control.Monad.Reader
import qualified Meatbars as MB
import           PeopleApi (PeopleApi, peopleServer)
import           Servant

type MeatbarApi = PeopleApi
             :<|> MeatbarsApi

type MeatbarsApi = "meatbars" :> "consumption" :> Get '[JSON] [EatenBar]
              :<|> "meatbars" :> "streaks" :> Get '[JSON] [EatStreak]

meatbarApi :: Proxy MeatbarApi
meatbarApi = Proxy

meatbarServer :: ServerT MeatbarApi App
meatbarServer = peopleServer
           :<|> getEatenMeatbars
           :<|> getEatStreaks

getEatenMeatbars :: App [EatenBar]
getEatenMeatbars = getAllEatenMeatbars >>= \eatenBarModels ->
  return $ map toEatenBar eatenBarModels

getEatStreaks :: App [EatStreak]
getEatStreaks = getAllEatenMeatbars >>= return . toEatStreaks . MB.findStreaks

getAllEatenMeatbars :: App [MB.EatenMeatbar]
getAllEatenMeatbars = reader getDBPool >>= \dbPool ->
  (liftIO $ MB.selectAllEatenMeatbars dbPool)
