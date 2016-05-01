{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MeatbarsSpec where

import           Data.Maybe (fromJust)
import           Data.Time
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Comparing UTCTimes" $ do
    it "compares UTCTimes" $ do
      (utctDay earlyDate == utctDay earlyDate) `shouldBe` True
      (utctDay laterDate == utctDay laterDate) `shouldBe` True
      (compare (utctDay earlyDate) (utctDay laterDate)) `shouldBe` LT
      (compare (utctDay laterDate) (utctDay earlyDate)) `shouldBe` GT

earlyDate :: UTCTime
earlyDate =
  fromJust $ parseTimeM False defaultTimeLocale "%FT%H:%M:%S%Q%Z" "1999-12-31T23:59:00.000"

laterDate :: UTCTime
laterDate =
  fromJust $ parseTimeM False defaultTimeLocale "%FT%H:%M:%S%Q%Z" "2000-01-01T00:00:00.000"
