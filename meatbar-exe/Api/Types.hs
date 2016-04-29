{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Types
( Person (..)
, samplePerson1
, samplePerson2
) where

import           Control.Monad (mzero)
import           Data.Aeson
import           Database.Persist.Sql(toSqlKey, SqlBackend, ToBackendKey, Key(..))
import           Data.Text (Text)
import qualified Models as M
import qualified Servant.Docs as SD

data Person = Person { getId :: M.PersonId
                     , getName :: Text
                     }
  deriving (Show, Eq)

instance ToJSON Person where
  toJSON (Person pId pName) =
    object [ "id"   .= pId
           , "name" .= pName
           ]

instance FromJSON Person where
  parseJSON (Object v) =
    Person <$>
      v .: "id" <*>
      v .: "name"
  parseJSON _ = mzero

instance SD.ToSample [Person] [Person] where
  toSample _ = Just $ [ samplePerson1, samplePerson2 ]

samplePerson1 :: Person
samplePerson1 = Person (toKey (1::Integer)) "Liz Lemon"

samplePerson2 :: Person
samplePerson2 = Person (toKey (2::Integer)) "Tracy Jordan"

toKey :: (Integral i, ToBackendKey SqlBackend record) => i -> Key record
toKey = toSqlKey . fromIntegral
