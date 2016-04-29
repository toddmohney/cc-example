{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Documentation.Types
( DocumentationApi
) where

import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Media ((//), (/:))
import           Servant as S

data Markdown

instance S.Accept Markdown where
    contentType _ = "text" // "markdown" /: ("charset", "utf-8")

instance S.MimeRender Markdown Text where
    mimeRender _ text = LBS.fromStrict (encodeUtf8 text)

type DocumentationApi = "docs" :> Get '[Markdown] Text
