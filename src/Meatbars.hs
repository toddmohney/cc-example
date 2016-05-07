{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Meatbars
( EatenMeatbar (..)
, Month
, DayOfMonth
, Streak (..)
, activityByMonth
, createEatenBar
, findOrCreateMeatbar
, findStreaks
, mostPopularDayOfMonth
, selectAllMeatbars
, selectAllEatenMeatbars
) where

import           Control.Monad.Logger (runStderrLoggingT)
import           Data.List (filter, groupBy, maximumBy, sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Time (UTCTime, toGregorian, utctDay)
import           Database
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import           Database.Persist.Sql
import           Database.Persist.Sqlite as DB
import qualified Models as M
import           Streak (Streak (..))
import           Streak as S

data EatenMeatbar =
  EatenMeatbar { getPerson :: Entity M.Person
               , getMeatbar :: Entity M.Meatbar
               , getEatenBar :: Entity M.EatenBar
               }
  deriving (Show, Eq)

type Month = Int
type DayOfMonth = Int

instance Ord EatenMeatbar where
  compare bar1 bar2 = compare (dateEaten bar1) (dateEaten bar2)

selectAllMeatbars :: WithDB [Entity M.Meatbar]
selectAllMeatbars = withDBConn $ DB.selectList ([] :: [DB.Filter M.Meatbar]) []

selectMeatbar :: M.Meatbar -> WithDB (Maybe (Entity M.Meatbar))
selectMeatbar meatbar = withDBConn $ DB.selectFirst [M.MeatbarName ==. M.meatbarName meatbar] []

selectAllEatenMeatbars :: WithDB [EatenMeatbar]
selectAllEatenMeatbars =
  withDBConn query >>= (return . (fmap toEatenMeatbar))
  where
    query =
      E.select $ E.from $ \(eatenBar `E.InnerJoin` person `E.InnerJoin` meatbar) -> do
        E.on $ eatenBar ^. M.EatenBarPersonId E.==. person ^. M.PersonId
        E.on $ eatenBar ^. M.EatenBarMeatbarId E.==. meatbar ^. M.MeatbarId
        return (eatenBar, person, meatbar)

selectEatenMeatbar :: M.EatenBar -> WithDB (Maybe (Entity M.EatenBar))
selectEatenMeatbar eatenBar =
  let query = DB.selectFirst [ M.EatenBarPersonId ==. M.eatenBarPersonId eatenBar
                             , M.EatenBarMeatbarId ==. M.eatenBarMeatbarId eatenBar
                             , M.EatenBarDateEaten ==. M.eatenBarDateEaten eatenBar
                             ]
                             []
  in withDBConn query

findOrCreateMeatbar :: M.Meatbar -> WithDB M.MeatbarId
findOrCreateMeatbar meatbar = selectMeatbar meatbar >>= \existingMeatbar ->
  case existingMeatbar of
    Nothing  -> withDBConn $ DB.insert meatbar
    (Just m) -> return . DB.entityKey $ m

createEatenBar :: M.EatenBar -> WithDB M.EatenBarId
createEatenBar eatenBar = selectEatenMeatbar eatenBar >>= \existingBar ->
  case existingBar of
    Nothing  -> withDBConn $ DB.insert eatenBar
    (Just b) -> return . DB.entityKey $ b

toEatenMeatbar :: (E.Entity M.EatenBar, E.Entity M.Person, E.Entity M.Meatbar) -> EatenMeatbar
toEatenMeatbar (eatenBar, person, meatbar) =
  EatenMeatbar person meatbar eatenBar

activityByMonth :: [EatenMeatbar] -> Map Month [EatenMeatbar]
activityByMonth = foldl (\map bar -> Map.insertWith (++) (monthEaten bar) [bar] map) Map.empty

mostPopularDayOfMonth :: Map Month [EatenMeatbar] -> Map Month (DayOfMonth, [EatenMeatbar])
mostPopularDayOfMonth = Map.foldlWithKey findMostPopularDay Map.empty

findMostPopularDay :: Map Month (DayOfMonth, [EatenMeatbar])
                      -> Month
                      -> [EatenMeatbar]
                      -> Map Month (DayOfMonth, [EatenMeatbar])
findMostPopularDay acc month barsEaten =
  let mostActiveDay = longest . groupByDayOfMonthEaten . sort $ barsEaten
      dayOfMonth    = dayEaten . head $ mostActiveDay
      longest       = maximumBy (\a b -> compare (length a) (length b))
  in
    Map.insert month (dayOfMonth, mostActiveDay) acc

findStreaks :: [EatenMeatbar] -> [Streak EatenMeatbar]
findStreaks = (S.filterStreaks 2) . S.collectStreaks . groupByDateEaten . sort

groupByDateEaten :: [EatenMeatbar] -> [[EatenMeatbar]]
groupByDateEaten =
  groupBy (\bar1 bar2 -> (utctDay . dateEaten $ bar1) == (utctDay . dateEaten $ bar2))

groupByDayOfMonthEaten :: [EatenMeatbar] -> [[EatenMeatbar]]
groupByDayOfMonthEaten =
  groupBy (\bar1 bar2 -> (dayEaten bar1) == (dayEaten bar2))

dateEaten :: EatenMeatbar -> UTCTime
dateEaten = M.eatenBarDateEaten . DB.entityVal . getEatenBar

monthEaten :: EatenMeatbar -> Month
monthEaten = sndOfThree . toGregorian . utctDay . dateEaten

dayEaten :: EatenMeatbar -> DayOfMonth
dayEaten = thirdOfThree . toGregorian . utctDay . dateEaten

sndOfThree :: (a,b,c) -> b
sndOfThree (_, b, _) = b

thirdOfThree :: (a,b,c) -> c
thirdOfThree (_, _, c) = c
