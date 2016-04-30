module Import.Import
( importData
) where

import           Control.Monad (forM_)
import           Database.Persist.Sql
import qualified Import.Extract as Ex
import qualified Meatbars as MB
import qualified Models as M
import qualified People as P

importData :: ConnectionPool -> IO ()
importData pool =
  Ex.parseData >>= \meatbarEaters -> do
    forM_ (meatbarEatersToPeople meatbarEaters) (P.createPerson pool)
    forM_ (meatbarEatersToMeatbars meatbarEaters) (MB.createMeatbar pool)
    return ()

meatbarEatersToPeople :: [Ex.MeatbarEater] -> [M.Person]
meatbarEatersToPeople eaters = map toPerson (Ex.getUniquePeople eaters)
  where
    toPerson (Ex.PersonName pName) = M.Person pName

meatbarEatersToMeatbars :: [Ex.MeatbarEater] -> [M.Meatbar]
meatbarEatersToMeatbars eaters = map toMeatbar (Ex.getUniqueMeatbars eaters)
  where
    toMeatbar (Ex.MeatbarName pName) = M.Meatbar pName
