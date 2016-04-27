module AppConfig
( AppConfig (..)
, getAppConfig
) where

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai                          (Middleware)

data AppConfig = AppConfig { getRequestLogger :: Middleware }

getAppConfig :: AppConfig
getAppConfig = AppConfig { getRequestLogger = logStdoutDev }
