{-# language TemplateHaskell #-}
module Text.JS where

import Data.FileEmbed (embedFile)
import qualified Data.Text as T (Text)
import Data.Text.Encoding (decodeUtf8)

liveJS :: T.Text
liveJS = decodeUtf8 $ $(embedFile "./vendor/js/live.js")



