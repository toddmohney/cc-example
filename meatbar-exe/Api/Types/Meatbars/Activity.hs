{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Types.Meatbars.Activity
( MonthlyActivity (..)
, toMonthlyActivity
) where

import           Api.Types.Meatbars.EatenBar (EatenBar (..), toEatenBar, sampleEatenBar1, sampleEatenBar2)
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map as Map
import           Meatbars (EatenMeatbar, Month, DayOfMonth)
import qualified Servant.Docs as SD

data MonthlyActivity =
  MonthlyActivity { getMonth :: Month
                  , getMostActiveDay :: DayOfMonth
                  , getMeatbars :: [EatenBar]
                  }
  deriving (Show, Eq)

instance ToJSON MonthlyActivity where
  toJSON (MonthlyActivity month activeDay eatenBars) =
    object [ "month"         .= month
           , "mostActiveDay" .= activeDay
           , "eatenBars"     .= eatenBars
           ]

instance FromJSON MonthlyActivity where
  parseJSON (Object v) =
    MonthlyActivity
      <$> v .: "month"
      <*> v .: "mostActiveDay"
      <*> v .: "eatenBars"
  parseJSON _ = mzero

instance SD.ToSample [MonthlyActivity] [MonthlyActivity] where
  toSample _ = Just $ [ sampleMonthlyActivity1, sampleMonthlyActivity2 ]

toMonthlyActivity :: Map Month (DayOfMonth, [EatenMeatbar]) -> [MonthlyActivity]
toMonthlyActivity = Map.foldlWithKey buildMonthlyActivity []

buildMonthlyActivity :: [MonthlyActivity]
                     -> Month
                     -> (DayOfMonth, [EatenMeatbar])
                     -> [MonthlyActivity]
buildMonthlyActivity acc month (dayOfMonth, eatenBars) =
  acc ++ [MonthlyActivity month dayOfMonth (map toEatenBar eatenBars)]

sampleMonthlyActivity1 :: MonthlyActivity
sampleMonthlyActivity1 = MonthlyActivity 1 3 [sampleEatenBar1, sampleEatenBar2]

sampleMonthlyActivity2 :: MonthlyActivity
sampleMonthlyActivity2 = MonthlyActivity 4 12 [sampleEatenBar1, sampleEatenBar2]
