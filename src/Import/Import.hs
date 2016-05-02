module Import.Import
( importData
) where

import           Control.Monad (forM_)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           Database.Persist.Sql as DB
import           Database.Persist.Sqlite as DB
import qualified Import.Extract as Ex
import qualified Meatbars as MB
import qualified Models as M
import qualified People as P

importData :: ConnectionPool -> IO ()
importData pool =
  Ex.parseData >>= \meatbarEaters -> do
    forM_ (meatbarEatersToPeople meatbarEaters) (P.findOrCreatePerson pool)
    forM_ (meatbarEatersToMeatbars meatbarEaters) (MB.findOrCreateMeatbar pool)

    eatenBars <- meatbarEatersToEatenBars pool meatbarEaters
    forM_ eatenBars (MB.createEatenBar pool)

    return ()

meatbarEatersToPeople :: [Ex.MeatbarEater] -> [M.Person]
meatbarEatersToPeople eaters = map toPerson (Ex.getUniquePeople eaters)
  where
    toPerson (Ex.PersonName pName) = M.Person pName

meatbarEatersToMeatbars :: [Ex.MeatbarEater] -> [M.Meatbar]
meatbarEatersToMeatbars eaters = map toMeatbar (Ex.getUniqueMeatbars eaters)
  where
    toMeatbar (Ex.MeatbarName pName) = M.Meatbar pName

meatbarEatersToEatenBars :: ConnectionPool -> [Ex.MeatbarEater] -> IO [M.EatenBar]
meatbarEatersToEatenBars pool eaters = do
  people   <- peopleLookup pool
  meatbars <- meatbarLookup pool
  return $ map (buildEater people meatbars) eaters

peopleLookup :: ConnectionPool -> IO [(Text, M.PersonId)]
peopleLookup pool =
  P.selectAllPeople pool >>= return . buildPeopleLookup

buildPeopleLookup :: [DB.Entity M.Person] -> [(Text, M.PersonId)]
buildPeopleLookup =
  map (\pEnt -> ((M.personName . DB.entityVal) pEnt, DB.entityKey pEnt))

meatbarLookup :: ConnectionPool -> IO [(Text, M.MeatbarId)]
meatbarLookup pool =
  MB.selectAllMeatbars pool >>= return . buildMeatbarLookup

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
