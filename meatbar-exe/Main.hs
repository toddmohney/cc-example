{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           App
import           AppConfig (AppConfig (..), getAppConfig)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans.Either (EitherT)
import qualified Database as DB
import qualified Documentation.Server as Docs
import           Documentation.Types (DocumentationApi)
import           MeatbarApi (MeatbarApi, meatbarServer)
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.Cors
import qualified Import.Import as Im
import           Servant

type Api = "api" :> MeatbarApi
      :<|> "api" :> DocumentationApi

main :: IO ()
main = getAppConfig >>= \appConfig -> do
  runReaderT (DB.runDB Im.importData) (getDBPool appConfig)
  Warp.run 8081 (app appConfig)

app :: AppConfig -> Wai.Application
app cfg =
  (getRequestLogger cfg)
  $ cors (const $ Just corsPolicy)
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

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let allowedMethods = simpleMethods
      allowedHeaders = ["Content-Type"]
  in
    simpleCorsResourcePolicy { corsMethods = allowedMethods
                             , corsRequestHeaders = allowedHeaders
                             }
