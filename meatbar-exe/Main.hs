{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           App
import           AppConfig (AppConfig (..), getAppConfig)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans.Either (EitherT)
import qualified Documentation.Server as Docs
import           Documentation.Types (DocumentationApi)
import           Meatbar (MeatbarApi, meatbarServer)
import qualified People
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp
import           Servant

type Api = "api" :> MeatbarApi
      :<|> "api" :> DocumentationApi

main :: IO ()
main =
  let appConfig = getAppConfig
  in Warp.run 8081 (app appConfig)

app :: AppConfig -> Wai.Application
app cfg =
  (getRequestLogger cfg)
  $ serve api (readerServer cfg)

api :: Proxy Api
api = Proxy

server :: ServerT Api App
server = meatbarServer
    :<|> Docs.documentationServer

readerServer :: AppConfig -> Server Api
readerServer cfg =
  enter (readerToEither cfg) server

readerToEither :: AppConfig -> App :~> EitherT ServantErr IO
readerToEither cfg =
  Nat $ \x -> runReaderT x cfg
