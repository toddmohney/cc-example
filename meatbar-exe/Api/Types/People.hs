{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Types.People
( Person (..)
, samplePerson1
, samplePerson2
, toPerson
) where

import           Control.Monad (mzero)
import           Data.Aeson
import           Database.Persist.Sql(toSqlKey, SqlBackend, ToBackendKey, Key(..))
import qualified Database.Persist.Sql as DB
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

toPerson :: DB.Entity M.Person -> Person
toPerson p =
  let person   = DB.entityVal p
      personId = DB.entityKey p
  in Person personId (M.personName person)

samplePerson1 :: Person
samplePerson1 = Person (toKey (1::Integer)) "Liz Lemon"

samplePerson2 :: Person
samplePerson2 = Person (toKey (2::Integer)) "Tracy Jordan"

toKey :: (Integral i, ToBackendKey SqlBackend record) => i -> Key record
toKey = toSqlKey . fromIntegral
