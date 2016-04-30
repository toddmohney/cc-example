{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Import.Extract
( PersonName (..)
, MeatbarName (..)
, MeatbarEater (..)
, parseData
, getUniquePeople
, getUniqueMeatbars
) where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.List (groupBy, sortBy)
import           Data.Text (Text)
import qualified Data.Vector as V
import           Data.Time.Clock (UTCTime)
import           Paths_front_row_screen


-- extract

data MeatbarEater =
  MeatbarEater { eaterName :: Text
               , barType :: Text
               }
  deriving (Show, Eq, Ord)

instance FromNamedRecord MeatbarEater where
  parseNamedRecord r =
    MeatbarEater
      <$> r .: "person"
      <*> r .: "meat-bar-type"

parseData :: IO [MeatbarEater]
parseData =
  decodeCSV >>= \result ->
    case result of
      Left err -> putStrLn err >> return []
      Right (_, v) -> return $ V.toList v

decodeCSV :: IO (Either String (Header, V.Vector MeatbarEater))
decodeCSV =
  dataFilePath >>= BL.readFile >>= return . decodeByName

dataFilePath :: IO FilePath
dataFilePath = getDataFileName "data/meatbar-data.csv"

-- transform

newtype PersonName = PersonName Text
  deriving (Show, Eq, Ord)

newtype MeatbarName = MeatbarName Text
  deriving (Show, Eq, Ord)

getUniquePeople :: [MeatbarEater] -> [PersonName]
getUniquePeople eaters =
  foldl takePersonName [] (groupByName $ sortBy compareByName eaters)

getUniqueMeatbars :: [MeatbarEater] -> [MeatbarName]
getUniqueMeatbars eaters =
  foldl takeMeatbarName [] (groupByMeatbar $ sortBy compareByMeatbar eaters)

takePersonName :: [PersonName] -> [MeatbarEater] -> [PersonName]
takePersonName acc a =
  case a of
    [] -> acc
    (x:_) -> (PersonName $ eaterName x):acc

takeMeatbarName :: [MeatbarName] -> [MeatbarEater] -> [MeatbarName]
takeMeatbarName acc a =
  case a of
    [] -> acc
    (x:_) -> (MeatbarName $ barType x):acc

compareByName :: MeatbarEater -> MeatbarEater -> Ordering
compareByName a b = compare (eaterName a) (eaterName b)

compareByMeatbar :: MeatbarEater -> MeatbarEater -> Ordering
compareByMeatbar a b = compare (barType a) (barType b)

groupByName :: [MeatbarEater] -> [[MeatbarEater]]
groupByName = groupBy (\a b -> (eaterName a) == (eaterName b))

groupByMeatbar :: [MeatbarEater] -> [[MeatbarEater]]
groupByMeatbar = groupBy (\a b -> (barType a) == (barType b))

