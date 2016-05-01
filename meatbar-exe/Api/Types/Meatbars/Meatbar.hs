{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Types.Meatbars.Meatbar
( Meatbar (..)
, sampleMeatbar1
, sampleMeatbar2
, toMeatbar
) where

import           Control.Monad (mzero)
import           Data.Aeson
import           Database.Persist.Sql(toSqlKey, SqlBackend, ToBackendKey, Key(..))
import qualified Database.Persist.Sql as DB
import           Data.Text (Text)
import qualified Models as M

data Meatbar =
  Meatbar { getId :: M.MeatbarId
          , getName :: Text
          }
  deriving (Show, Eq)

instance ToJSON Meatbar where
  toJSON (Meatbar mbId mbName) =
    object [ "id"   .= mbId
           , "name" .= mbName
           ]

instance FromJSON Meatbar where
  parseJSON (Object v) =
    Meatbar <$>
      v .: "id" <*>
      v .: "name"
  parseJSON _ = mzero

toMeatbar :: DB.Entity M.Meatbar -> Meatbar
toMeatbar mb =
  let meatbar   = DB.entityVal mb
      meatbarId = DB.entityKey mb
  in Meatbar meatbarId (M.meatbarName meatbar)

sampleMeatbar1 :: Meatbar
sampleMeatbar1 =
  Meatbar (toKey (1::Integer)) "lobster"

sampleMeatbar2 :: Meatbar
sampleMeatbar2 =
  Meatbar (toKey (1::Integer)) "raccoon"

toKey :: (Integral i, ToBackendKey SqlBackend record) => i -> Key record
toKey = toSqlKey . fromIntegral


