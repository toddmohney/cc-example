{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MeatbarsSpec where

import           Data.Maybe (fromJust)
import           Data.Time
import qualified Database.Persist.Sql as DB
import qualified Database.Persist.Sqlite as DB
import           Meatbars
import qualified Models as M
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let one = 1 :: Int
  let two = 2 :: Int
  let three = 3 :: Int
  let four = 4 :: Int
  let five = 5 :: Int
  let six = 6 :: Int
  let seven = 7 :: Int

  describe "Comparing UTCTimes" $ do
    it "compares UTCTimes" $ do
      (utctDay earlyDate == utctDay earlyDate) `shouldBe` True
      (utctDay laterDate == utctDay laterDate) `shouldBe` True
      (compare (utctDay earlyDate) (utctDay laterDate)) `shouldBe` LT
      (compare (utctDay laterDate) (utctDay earlyDate)) `shouldBe` GT

  describe "collectStreaks" $ do
    it "returns an empty list when given empty input (base case)" $ do
      collectStreaks emptyNestedList `shouldBe` emptyStreak

    it "returns a streak of one element when given a single element as input" $ do
      collectStreaks [[one]] `shouldBe` [ Streak [[one]] ]

    describe "capturing a streak" $ do
      it "collects elements in a streak when the successive input lengths are increasing" $ do
        collectStreaks [ [one]
                       , [two,two]
                       , [three,three,three]
                       ]
          `shouldBe` [ Streak [ [one]
                              , [two,two]
                              , [three,three,three]
                              ]
                     ]

    describe "resetting the streak" $ do
      it "resets the streak when successive input lengths are not increasing" $ do
        collectStreaks [[one,one], [two]] `shouldBe` [ Streak [[one,one]], Streak [[two]] ]

    describe "a full example" $ do
      it "collects streaks" $ do
        collectStreaks [ [one]
                       , [two,two]
                       , [three,three,three]
                       , [four]
                       , [five,five]
                       , [six]
                       , [seven]
                       ]
          `shouldBe` [ Streak [[one], [two,two], [three,three,three]]
                     , Streak [[four], [five,five]]
                     , Streak [[six]]
                     , Streak [[seven]]
                     ]

  describe "filterStreaks" $ do
    it "discards any Streaks with a lenght below the minimum" $ do
      let streak = [ Streak [[one], [two,two], [three,three,three]]
                   , Streak [[six]]
                   , Streak [[four], [five,five]]
                   , Streak [[seven]]
                   ]
      let expectedResult = [ Streak [[one], [two,two], [three,three,three]]
                           , Streak [[four], [five,five]]
                           ]
      filterStreaks 2 streak `shouldBe` expectedResult



  {- describe "Ord instance for EatenMeatbar" $ do -}
    {- it "compares the dateEaten value" $ do -}
      {- let earlyMeatbar = EatenMeatbar dbPerson dbMeatbar earlyMeatbar -}
      {- let laterMeatbar = EatenMeatbar dbPerson dbMeatbar laterMeatbar -}
      {- (compare earlyMeatbar laterMeatbar) `shouldBe` LT -}
      {- (compare laterMeatbar earlyMeatbar) `shouldBe` GT -}
      {- (compare earlyMeatbar earlyMeatbar) `shouldBe` EQ -}

{- earlyMeatbar :: DB.Entity M.EatenBar -}
{- earlyMeatbar = -}
  {- DB.Entity (M.EatenBar (toKey (1::Integer)) (toKey (2::Integer)) earlyDate) -}

{- laterMeatbar :: DB.Entity M.EatenBar -}
{- laterMeatbar = -}
  {- DB.Entity (M.EatenBar (toKey (1::Integer)) (toKey (2::Integer)) laterDate) -}

emptyNestedList :: [[Int]]
emptyNestedList = []

emptyList :: [Int]
emptyList = []

emptyStreak :: [Streak Int]
emptyStreak = []

earlyDate :: UTCTime
earlyDate =
  fromJust $ parseTimeM False defaultTimeLocale "%FT%H:%M:%S%Q%Z" "1999-12-31T23:59:00.000"

laterDate :: UTCTime
laterDate =
  fromJust $ parseTimeM False defaultTimeLocale "%FT%H:%M:%S%Q%Z" "2000-01-01T00:00:00.000"

{- dbPerson :: DB.Entity M.Person -}
{- dbPerson = DB.Entity (M.Person "chewbacca") -}

{- dbMeatbar :: DB.Entity M.Meatbar -}
{- dbMeatbar = DB.Entity (M.Meatbar "rattlesnake") -}

{- toKey :: (Integral i, DB.ToBackendKey DB.SqlBackend record) => i -> DB.Key record -}
{- toKey = DB.toSqlKey . fromIntegral -}
