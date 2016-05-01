{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Meatbars
( EatenMeatbar (..)
, Streak (..)
, createMeatbar
, createEatenBar
, findStreaks
, collectStreaks
, filterStreaks
, selectAllMeatbars
, selectAllEatenMeatbars
) where

import           Control.Monad.Logger (runStderrLoggingT)
import           Data.List (filter, groupBy, sort)
import           Data.Text (Text)
import           Data.Time (UTCTime, utctDay)
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import           Database.Persist.Sql
import           Database.Persist.Sqlite as DB
import qualified Models as M

data EatenMeatbar =
  EatenMeatbar { getPerson :: Entity M.Person
               , getMeatbar :: Entity M.Meatbar
               , getEatenBar :: Entity M.EatenBar
               }
  deriving (Show, Eq)

newtype Streak a = Streak [[a]]
  deriving (Show, Eq, Ord)

instance Ord EatenMeatbar where
  compare bar1 bar2 = compare (dateEaten bar1) (dateEaten bar2)

dateEaten :: EatenMeatbar -> UTCTime
dateEaten = M.eatenBarDateEaten . DB.entityVal . getEatenBar

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

findStreaks :: [EatenMeatbar] -> [Streak EatenMeatbar]
findStreaks = (filterStreaks 2) . collectStreaks . groupByDateEaten . sort

groupByDateEaten :: [EatenMeatbar] -> [[EatenMeatbar]]
groupByDateEaten =
  groupBy (\bar1 bar2 -> (utctDay . dateEaten $ bar1) == (utctDay . dateEaten $ bar2))

collectStreaks :: [[a]] -> [Streak a]
collectStreaks []       = []
collectStreaks [x]      = [Streak [x]]
collectStreaks (x:y:zs) = collectStreaks' (x:y:zs) (Streak [x]) []

collectStreaks' :: [[a]] -> Streak a -> [Streak a] -> [Streak a]
collectStreaks' [] currStreak streaks     = streaks ++ [currStreak]
collectStreaks' (x:[]) currStreak streaks = streaks ++ [currStreak]
collectStreaks' (x:y:zs) currStreak streaks
  | (length y) > (length x) = collectStreaks' (y:zs) (appendToStreak y currStreak) streaks
  | otherwise = collectStreaks' (y:zs) (Streak [y]) (streaks ++ [currStreak])

filterStreaks :: Int -> [Streak a] -> [Streak a]
filterStreaks minStreakLength = filter (\(Streak a) -> length a >= minStreakLength)

appendToStreak :: [a] -> Streak a -> Streak a
appendToStreak a (Streak s) = (Streak (s ++ [a]))

createMeatbar :: ConnectionPool -> M.Meatbar -> IO M.MeatbarId
createMeatbar pool meatbar =
  runStderrLoggingT $ runSqlPool (DB.insert meatbar) pool

createEatenBar :: ConnectionPool -> M.EatenBar -> IO M.EatenBarId
createEatenBar pool eatenBar =
  runStderrLoggingT $ runSqlPool (DB.insert eatenBar) pool

toEatenMeatbar :: (E.Entity M.EatenBar, E.Entity M.Person, E.Entity M.Meatbar) -> EatenMeatbar
toEatenMeatbar (eatenBar, person, meatbar) =
  EatenMeatbar person meatbar eatenBar
