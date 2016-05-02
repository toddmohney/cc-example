{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Meatbars
( EatenMeatbar (..)
, Month
, DayOfMonth
, Streak (..)
, activityByMonth
, createMeatbar
, createEatenBar
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

selectAllMeatbars :: ConnectionPool -> IO [Entity M.Meatbar]
selectAllMeatbars pool =
  let query = DB.selectList ([] :: [DB.Filter M.Meatbar]) []
  in runStderrLoggingT $ runSqlPool query pool

selectAllEatenMeatbars :: ConnectionPool -> IO [EatenMeatbar]
selectAllEatenMeatbars pool =
  (runStderrLoggingT $ runSqlPool query pool) >>= (return . (fmap toEatenMeatbar))
  where
    query =
      E.select $ E.from $ \(eatenBar `E.InnerJoin` person `E.InnerJoin` meatbar) -> do
        E.on $ eatenBar ^. M.EatenBarPersonId E.==. person ^. M.PersonId
        E.on $ eatenBar ^. M.EatenBarMeatbarId E.==. meatbar ^. M.MeatbarId
        return (eatenBar, person, meatbar)

createMeatbar :: ConnectionPool -> M.Meatbar -> IO M.MeatbarId
createMeatbar pool meatbar =
  runStderrLoggingT $ runSqlPool (DB.insert meatbar) pool

createEatenBar :: ConnectionPool -> M.EatenBar -> IO M.EatenBarId
createEatenBar pool eatenBar =
  runStderrLoggingT $ runSqlPool (DB.insert eatenBar) pool

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
