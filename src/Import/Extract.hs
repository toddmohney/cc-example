{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Import.Extract
( PersonName (..)
, MeatbarName (..)
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

data MeatBarEater =
  MeatBarEater { eaterName :: Text
               , barType :: Text
               }
  deriving (Show, Eq, Ord)

instance FromNamedRecord MeatBarEater where
  parseNamedRecord r =
    MeatBarEater
      <$> r .: "person"
      <*> r .: "meat-bar-type"

parseData :: IO [MeatBarEater]
parseData =
  decodeCSV >>= \result ->
    case result of
      Left err -> putStrLn err >> return []
      Right (_, v) -> return $ V.toList v

decodeCSV :: IO (Either String (Header, V.Vector MeatBarEater))
decodeCSV =
  dataFilePath >>= BL.readFile >>= return . decodeByName

dataFilePath :: IO FilePath
dataFilePath = getDataFileName "data/meatbar-data.csv"

-- transform

newtype PersonName = PersonName Text
  deriving (Show, Eq, Ord)

newtype MeatbarName = MeatbarName Text
  deriving (Show, Eq, Ord)

getUniquePeople :: [MeatBarEater] -> [PersonName]
getUniquePeople eaters =
  foldl takePersonName [] (groupByName $ sortBy compareByName eaters)

getUniqueMeatbars :: [MeatBarEater] -> [MeatbarName]
getUniqueMeatbars eaters =
  foldl takeMeatbarName [] (groupByMeatbar $ sortBy compareByMeatbar eaters)

takePersonName :: [PersonName] -> [MeatBarEater] -> [PersonName]
takePersonName acc a =
  case a of
    [] -> acc
    (x:_) -> (PersonName $ eaterName x):acc

takeMeatbarName :: [MeatbarName] -> [MeatBarEater] -> [MeatbarName]
takeMeatbarName acc a =
  case a of
    [] -> acc
    (x:_) -> (MeatbarName $ barType x):acc

compareByName :: MeatBarEater -> MeatBarEater -> Ordering
compareByName a b = compare (eaterName a) (eaterName b)

compareByMeatbar :: MeatBarEater -> MeatBarEater -> Ordering
compareByMeatbar a b = compare (barType a) (barType b)

groupByName :: [MeatBarEater] -> [[MeatBarEater]]
groupByName = groupBy (\a b -> (eaterName a) == (eaterName b))

groupByMeatbar :: [MeatBarEater] -> [[MeatBarEater]]
groupByMeatbar = groupBy (\a b -> (barType a) == (barType b))

