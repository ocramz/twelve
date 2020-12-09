{-# options_ghc -Wno-unused-imports #-}
module Text.Html where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Control.Lens (Prism, (%%~), traversed)
import Data.Profunctor.Choice (Choice(..))
import Text.XML (Document, Element(..), Node(..))
import Text.Xml.Lens.LowLevel (_NodeContent)
