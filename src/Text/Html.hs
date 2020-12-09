{-# LANGUAGE OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Text.Html where

import Control.Applicative (Alternative (..), optional)
import Control.Applicative.Combinators (between, count, sepBy)
import Control.Monad (MonadPlus (..), join, void)
import Control.Monad.State.Lazy (MonadState (..), StateT (..), evalStateT, execStateT, runStateT)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (digitToInt, isAlpha, isDigit, isPunctuation)
import qualified Data.Text as T (Text, pack)
import Data.Void
import Text.Megaparsec
  ( MonadParsec (..),
    Parsec,
    ParsecT,
    parse,
    runParserT,
    satisfy,
  )
import Text.Megaparsec.Char (char, digitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, scientific, skipBlockComment, skipLineComment, space, symbol)
import Text.Megaparsec.Error (ErrorFancy (..), ParseError (..), ParseErrorBundle (..), errorBundlePretty)
import Text.Megaparsec.Pos (Pos, SourcePos, initialPos, mkPos)

import Data.ByteString (ByteString)


import Control.Lens (Prism, (%%~), traverseOf)
import Data.Profunctor.Choice (Choice(..))
import Text.XML (Document, Element(..), Node(..))
import Text.Xml.Lens.LowLevel (_NodeContent)



-- traverseOf _NodeContent
--   :: Applicative f => (Text -> f Text) -> Node -> f Node


stache :: Parser a -> Parser a
stache = between (symbol "{{") (symbol "}}")

squareBkts :: Parser a -> Parser a
squareBkts = between (symbol "[") (symbol "]")

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineC blockC
  where
    lineC = L.skipLineComment "#"
    blockC = L.skipBlockComment "##" "##"



-- | Simplified parser type for smaller type sigs
type Parser = Parsec Void T.Text

-- type ParserBS = Parsec Void BS.ByteString

type ParseE = ParseErrorBundle T.Text Void
