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
import           Api.Types.Meatbars.Activity (MonthlyActivity (..), toMonthlyActivity)
import           App
import           AppConfig
import           Control.Monad.Reader
import qualified Database as DB
import qualified Meatbars as MB
import           PeopleApi (PeopleApi, peopleServer)
import           Servant

type MeatbarApi = PeopleApi
             :<|> MeatbarsApi

type MeatbarsApi = "meatbars" :> "consumption" :> Get '[JSON] [EatenBar]
              :<|> "meatbars" :> "streaks" :> Get '[JSON] [EatStreak]
              :<|> "meatbars" :> "activity" :> Get '[JSON] [MonthlyActivity]

meatbarApi :: Proxy MeatbarApi
meatbarApi = Proxy

meatbarServer :: ServerT MeatbarApi App
meatbarServer = peopleServer
           :<|> getEatenMeatbars
           :<|> getEatStreaks
           :<|> getHighestDayByMonth

getEatenMeatbars :: App [EatenBar]
getEatenMeatbars = liftM (map toEatenBar) getAllEatenMeatbars

getEatStreaks :: App [EatStreak]
getEatStreaks = liftM  (toEatStreaks . MB.findStreaks) getAllEatenMeatbars

getHighestDayByMonth :: App [MonthlyActivity]
getHighestDayByMonth =
  liftM (toMonthlyActivity . MB.mostPopularDayOfMonth . MB.activityByMonth) getAllEatenMeatbars

getAllEatenMeatbars :: App [MB.EatenMeatbar]
getAllEatenMeatbars =
  let meatbarQuery = DB.runDB MB.selectAllEatenMeatbars
  in (reader getDBPool) >>= liftIO . (runReaderT meatbarQuery)
