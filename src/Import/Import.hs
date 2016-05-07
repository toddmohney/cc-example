module Import.Import
( importData
) where

import           Control.Monad (forM_, liftM)
import           Control.Monad.Reader (liftIO)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           Database
import           Database.Persist.Sql as DB
import           Database.Persist.Sqlite as DB
import qualified Import.Extract as Ex
import qualified Meatbars as MB
import qualified Models as M
import qualified People as P

importData :: WithDB ()
importData =
  (liftIO Ex.parseData) >>= \meatbarEaters -> do
    forM_ (meatbarEatersToPeople meatbarEaters) P.findOrCreatePerson
    forM_ (meatbarEatersToMeatbars meatbarEaters) MB.findOrCreateMeatbar

    eatenBars <- meatbarEatersToEatenBars meatbarEaters
    forM_ eatenBars MB.createEatenBar

meatbarEatersToPeople :: [Ex.MeatbarEater] -> [M.Person]
meatbarEatersToPeople eaters = map toPerson (Ex.getUniquePeople eaters)
  where
    toPerson (Ex.PersonName pName) = M.Person pName

meatbarEatersToMeatbars :: [Ex.MeatbarEater] -> [M.Meatbar]
meatbarEatersToMeatbars eaters = map toMeatbar (Ex.getUniqueMeatbars eaters)
  where
    toMeatbar (Ex.MeatbarName pName) = M.Meatbar pName

meatbarEatersToEatenBars :: [Ex.MeatbarEater] -> WithDB [M.EatenBar]
meatbarEatersToEatenBars eaters = do
  people   <- peopleLookup
  meatbars <- meatbarLookup
  return $ map (buildEater people meatbars) eaters

peopleLookup :: WithDB [(Text, M.PersonId)]
peopleLookup =
  liftM buildPeopleLookup P.selectAllPeople

buildPeopleLookup :: [DB.Entity M.Person] -> [(Text, M.PersonId)]
buildPeopleLookup =
  map (\pEnt -> ((M.personName . DB.entityVal) pEnt, DB.entityKey pEnt))

meatbarLookup :: WithDB [(Text, M.MeatbarId)]
meatbarLookup =
  liftM buildMeatbarLookup MB.selectAllMeatbars

buildMeatbarLookup :: [DB.Entity M.Meatbar] -> [(Text, M.MeatbarId)]
buildMeatbarLookup =
  map (\mbEnt -> ((M.meatbarName . DB.entityVal) mbEnt, DB.entityKey mbEnt))

buildEater :: [(Text, M.PersonId)]->
              [(Text, M.MeatbarId)] ->
              Ex.MeatbarEater ->
              M.EatenBar
buildEater personLookup meatbarLookup (Ex.MeatbarEater name typ date) =
  let personId  = fromJust $ lookup name personLookup
      meatbarId = fromJust $ lookup typ meatbarLookup
  in
    M.EatenBar personId meatbarId date
