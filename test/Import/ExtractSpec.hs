{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Import.ExtractSpec where

import           Import.Extract (MeatbarName (..), PersonName (..))
import qualified Import.Extract as Ex
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseData" $ do
    it "Hydrates all records from the CSV" $ do
      Ex.parseData >>= \meatbarEaters ->
        length meatbarEaters `shouldBe` 25

  describe "getUniquePeople" $ do
    it "Returns unique names of people" $ do
      Ex.parseData >>= \meatbarEaters -> do
        let uniquePeopleNames = Ex.getUniquePeople meatbarEaters

        length uniquePeopleNames `shouldBe` 3

        elem (PersonName "ashton") uniquePeopleNames `shouldBe` True
        elem (PersonName "bob") uniquePeopleNames `shouldBe` True
        elem (PersonName "chuck") uniquePeopleNames `shouldBe` True

  describe "getUniqueMeatbars" $ do
    it "Returns unique names of meatbars" $ do
      Ex.parseData >>= \meatbarEaters -> do
        let uniqueMeatbarNames = Ex.getUniqueMeatbars meatbarEaters

        length uniqueMeatbarNames `shouldBe` 3

        elem (MeatbarName "beef") uniqueMeatbarNames `shouldBe` True
        elem (MeatbarName "bison") uniqueMeatbarNames `shouldBe` True
        elem (MeatbarName "lamb") uniqueMeatbarNames `shouldBe` True
