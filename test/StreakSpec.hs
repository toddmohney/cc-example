{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module StreakSpec where

import Streak
import Test.Hspec

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

  describe "collectStreaks" $ do
    it "returns an empty list when given empty input (base case)" $ do
      collectStreaks emptyNestedList `shouldBe` emptyStreak

    it "returns a streak of one element when given a single element as input" $ do
      collectStreaks [[one]] `shouldBe` [ Streak [[one]] ]

    describe "capturing a streak" $ do
      it "collects elements in a streak when the successive input lengths are increasing" $ do
        collectStreaks [ [one] , [two,two] , [three,three,three] ]
          `shouldBe` [ Streak [ [one] , [two,two] , [three,three,three] ] ]

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


emptyNestedList :: [[Int]]
emptyNestedList = []

emptyStreak :: [Streak Int]
emptyStreak = []

