{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Documentation
( DocumentationApi
, documentationServer
) where

import App (App)
import Data.Text as Text (Text)
import Servant

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Media ((//), (/:))
import Servant as S

data Markdown

instance S.Accept Markdown where
    contentType _ = "text" // "markdown" /: ("charset", "utf-8")

instance S.MimeRender Markdown Text where
    mimeRender _ text = LBS.fromStrict (encodeUtf8 text)

type DocumentationApi = "docs" :> Get '[Markdown] Text

documentationServer :: App Text
documentationServer = return (Text.pack documentation)

documentation :: String
documentation = undefined
