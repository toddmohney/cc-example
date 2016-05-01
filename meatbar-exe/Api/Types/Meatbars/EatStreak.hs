{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Types.Meatbars.EatStreak
( EatStreak (..)
, sampleEatStreak1
, sampleEatStreak2
, toEatStreaks
) where

import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Maybe (fromJust, mapMaybe)
import           Data.List (sort)
import           Data.Time
import           Database.Persist.Sqlite as DB
import qualified Meatbars as MB
import qualified Models as M
import qualified Servant.Docs as SD

data EatStreak =
  EatStreak { getStartDate :: UTCTime
            , getEndDate :: UTCTime
            , getStreakDays :: Integer
            , getBarsEaten :: Int
            }
  deriving (Show, Eq)

instance ToJSON EatStreak where
  toJSON (EatStreak start end streakDays barsEaten) =
    object [ "startDate"   .= start
           , "endDate" .= end
           , "streakDays" .= streakDays
           , "barsEaten" .= barsEaten
           ]

instance FromJSON EatStreak where
  parseJSON (Object v) =
    EatStreak
      <$> v .: "startDate"
      <*> v .: "endDate"
      <*> v .: "streakDays"
      <*> v .: "barsEaten"
  parseJSON _ = mzero

instance SD.ToSample [EatStreak] [EatStreak] where
  toSample _ = Just $ [ sampleEatStreak1, sampleEatStreak2 ]

toEatStreaks :: [MB.Streak MB.EatenMeatbar] -> [EatStreak]
toEatStreaks []      = []
toEatStreaks streaks = mapMaybe toEatStreak streaks

toEatStreak :: MB.Streak MB.EatenMeatbar -> Maybe EatStreak
toEatStreak (MB.Streak [])     = Nothing
toEatStreak (MB.Streak [[]])   = Nothing
toEatStreak (MB.Streak streak) =
  let startDate = M.eatenBarDateEaten . DB.entityVal . MB.getEatenBar . head . sort . head $ streak
      endDate   = M.eatenBarDateEaten . DB.entityVal . MB.getEatenBar . last . sort . last $ streak
      streakLength = diffDays (utctDay endDate) (utctDay startDate)
      barsEaten = foldl (\acc s -> acc + (length s)) 0 streak
  in
    Just $ EatStreak startDate endDate streakLength barsEaten

sampleEatStreak1 :: EatStreak
sampleEatStreak1 =
  EatStreak sampleUTCTime1 sampleUTCTime1 3 14

sampleEatStreak2 :: EatStreak
sampleEatStreak2 =
  EatStreak sampleUTCTime1 sampleUTCTime1 5 21

sampleUTCTime1 :: UTCTime
sampleUTCTime1 =
  fromJust $ parseTimeM False defaultTimeLocale "%FT%H:%M:%S%Q%Z" "1999-12-31T23:59:00.000"

