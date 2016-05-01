{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Types.Meatbars.EatenBar
( EatenBar (..)
, sampleEatenBar1
, sampleEatenBar2
, toEatenBar
) where

import qualified Api.Types.Meatbars.Meatbar as Api
import           Api.Types.People (Person (..))
import qualified Api.Types.People as Api
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Maybe (fromJust)
import           Data.Time
import           Database.Persist.Sql(toSqlKey, SqlBackend, ToBackendKey, Key(..))
import qualified Database.Persist.Sql as DB
import qualified Meatbars as MB
import qualified Models as M
import qualified Servant.Docs as SD

data EatenBar =
  EatenBar { getId :: M.EatenBarId
           , getPerson :: Person
           , getMeatbar :: Api.Meatbar
           , dateEaten :: UTCTime
           }
  deriving (Show, Eq)

instance ToJSON EatenBar where
  toJSON (EatenBar ebId ebPerson ebMeatbar ebDateEaten) =
    object [ "id"   .= ebId
           , "eater" .= ebPerson
           , "meatbar" .= ebMeatbar
           , "dateEaten" .= ebDateEaten
           ]

instance FromJSON EatenBar where
  parseJSON (Object v) =
    EatenBar
      <$> v .: "id"
      <*> v .: "eater"
      <*> v .: "meatbar"
      <*> v .: "dateEaten"
  parseJSON _ = mzero

instance SD.ToSample [EatenBar] [EatenBar] where
  toSample _ = Just $ [ sampleEatenBar1, sampleEatenBar2 ]

toEatenBar :: MB.EatenMeatbar -> EatenBar
toEatenBar (MB.EatenMeatbar personEnt meatbarEnt eatenBarEnt) =
  EatenBar
    (DB.entityKey eatenBarEnt)
    (Api.toPerson personEnt)
    (Api.toMeatbar meatbarEnt)
    (M.eatenBarDateEaten . DB.entityVal $ eatenBarEnt)

sampleEatenBar1 :: EatenBar
sampleEatenBar1 =
  EatenBar (toKey (1::Integer)) Api.samplePerson1 Api.sampleMeatbar1 sampleUTCTime1

sampleEatenBar2 :: EatenBar
sampleEatenBar2 =
  EatenBar (toKey (2::Integer)) Api.samplePerson2 Api.sampleMeatbar2 sampleUTCTime2

sampleUTCTime1 :: UTCTime
sampleUTCTime1 =
  fromJust $ parseTimeM False defaultTimeLocale "%FT%H:%M:%S%Q%Z" "1999-12-31T23:59:00.000"

sampleUTCTime2 :: UTCTime
sampleUTCTime2 =
  fromJust $ parseTimeM False defaultTimeLocale "%FT%H:%M:%S%Q%Z" "2000-01-01T00:00:00.000"

toKey :: (Integral i, ToBackendKey SqlBackend record) => i -> Key record
toKey = toSqlKey . fromIntegral


