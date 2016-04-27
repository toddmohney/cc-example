{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           App
import           AppConfig (AppConfig (..), getAppConfig)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans.Either (EitherT)
import           People (PeopleApi)
import qualified People
import           Documentation (DocumentationApi)
import qualified Documentation as Docs
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp
import           Servant

type MeatbarApi = "api" :> PeopleApi
             :<|> "api" :> DocumentationApi

main :: IO ()
main =
  let appConfig = getAppConfig
  in Warp.run 8081 (app appConfig)

app :: AppConfig -> Wai.Application
app cfg =
  (getRequestLogger cfg)
  $ serve api (readerServer cfg)

api :: Proxy MeatbarApi
api = Proxy

server :: ServerT MeatbarApi App
server = People.peopleServer
    :<|> Docs.documentationServer

readerServer :: AppConfig -> Server MeatbarApi
readerServer cfg =
  enter (readerToEither cfg) server

readerToEither :: AppConfig -> App :~> EitherT ServantErr IO
readerToEither cfg =
  Nat $ \x -> runReaderT x cfg
