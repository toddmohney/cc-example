{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Documentation.Server
( documentationServer
) where

import           App (App)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Meatbar as MB
import qualified Servant.Docs as SD

documentationServer :: App Text
documentationServer = return (Text.pack documentation)

documentation :: String
documentation = SD.markdown (SD.docsWithIntros [intro] MB.meatbarApi)

intro :: SD.DocIntro
intro = SD.DocIntro "Meatbarrrr" ["Welcome to our API", "Feel free to dig around"]
