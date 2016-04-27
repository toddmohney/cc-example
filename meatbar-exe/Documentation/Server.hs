{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Documentation.Server
( documentationServer
) where

import           App (App)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Meatbar as MB
import           People (Person (..))
import           Servant as S
import qualified Servant.Docs as SD

documentationServer :: App Text
documentationServer = return (Text.pack documentation)

documentation :: String
documentation = undefined -- SD.markdown (SD.docsWithIntros [intro] MB.meatbarApi)

intro :: SD.DocIntro
intro = SD.DocIntro "Meatbarrrr" ["Welcome to our API", "Feel free to dig around"]

instance SD.ToSample [Person] [Person] where
  toSample _ = Just $ [ samplePerson1, samplePerson2 ]

samplePerson1 :: Person
samplePerson1 = Person "Liz Lemon"

samplePerson2 :: Person
samplePerson2 = Person "Tracy Jordan"

