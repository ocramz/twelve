{-# language TemplateHaskell #-}
{-# options_ghc -Wno-unused-imports #-}
module Text.JS where

import Data.FileEmbed (embedFile)
import qualified Data.Text as T (Text)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException(..))

liveJS :: T.Text
liveJS = decodeUtf8 $(embedFile "./vendor/js/live.js")
