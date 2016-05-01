{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Database.Persist.TH
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Person json sql=people
    name Text sqltype=text
    deriving Show Eq

  Meatbar json sql=meatbars
    name Text sqltype=text
    deriving Show Eq

  EatenBar json sql=eaten_bars
    personId PersonId
    meatbarId MeatbarId
    dateEaten UTCTime
    deriving Show Eq
|]
