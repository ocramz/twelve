{-# LANGUAGE OverloadedStrings #-}
{-# language LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Text.Html where

import Control.Applicative (Alternative (..), optional)
import Control.Applicative.Combinators (some, between, count, sepBy)
import Control.Monad (MonadPlus (..), join, void)
import Control.Monad.State.Lazy (MonadState (..), StateT (..), evalStateT, execStateT, runStateT)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (digitToInt, isAlpha, isDigit, isPunctuation, isSpace)
import Data.Functor.Contravariant (Contravariant(..))
import Data.String (IsString(..))

import Data.Void

-- megaparsec
import Text.Megaparsec ( MonadParsec (..), Parsec, ParsecT, parse, parseTest, runParserT, satisfy)
import Text.Megaparsec.Char (char, digitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, scientific, skipBlockComment, skipLineComment, space, symbol)
import Text.Megaparsec.Error (ErrorFancy (..), ParseError (..), ParseErrorBundle (..), errorBundlePretty)
import Text.Megaparsec.Pos (Pos, SourcePos, initialPos, mkPos)

-- bytestring
import qualified Data.ByteString.Lazy as LBS (ByteString, readFile)
-- containers
import qualified Data.Map as M (Map, fromList)
-- lens
-- import Control.Lens (Traversal', Prism, (^?), (^..), (...), over, (.~))
-- text
import qualified Data.Text as T (Text, all, pack, unpack)
import qualified Data.Text.IO as T (putStrLn)
import qualified Data.Text.Lazy as TL (toStrict, fromStrict)

import Text.XML (Document(..), Prologue(..), Element(..), Node(..), readFile, parseLBS, def, renderText, psDecodeEntities, decodeHtmlEntities, rsPretty, rsXMLDeclaration)
import Data.XML.Types (Name(..))
-- import Text.Xml.Lens (AsHtmlDocument(..), _HtmlDocument, html, name, text, texts)
-- import Text.Xml.Lens.LowLevel (_NodeContent)

import Prelude hiding (readFile)

doc :: LBS.ByteString
doc = "<html> <body>{{ card2.html }}</body></html>"

docParse :: Document
docParse =
  Document
    (Prologue [] Nothing [])
    (Element (Name "html" Nothing Nothing) mempty [
        NodeContent " ",
        NodeElement (Element (Name "body" Nothing Nothing) mempty [NodeContent "{{ card2.html }}"])
        ]
      ) []

hits = let (Document _ el _) = docParse in flip nodeContents el $ \t ->
  case parsePattern t of
    Right x -> do
      putStrLn x
      pure $ T.pack x
    Left e -> error $ errorBundlePretty e

-- 

loadAndProcess :: [FilePath] -- ^ absolute paths of HTML files in input dir
               -> FilePath -- ^ template file to be processed
               -> IO T.Text
loadAndProcess inPaths fp = do
  bs <- LBS.readFile fp
  let
    decSetts = def { psDecodeEntities = decodeHtmlEntities }
    encSetts = def { rsPretty = True, rsXMLDeclaration = False }
  case parseLBS decSetts bs of
    Left e -> error $ show e -- FIXME
    Right (Document dpre el dpost) -> do
      el' <- flip nodeContents el $ \t ->
        case parsePattern t of
          Right fpIn -> do -- NB fpIn must exist in the input dir
            if fpIn `elem` inPaths
              then do
                putStrLn $ unwords [fpIn, "found in", unwords inPaths]
                loadAndProcess inPaths fpIn
              else error $ unwords [fpIn, "does not occur in", unwords inPaths ]
          Left _ -> pure t -- if parse fails, return original node content
          -- Left e -> error $ errorBundlePretty e -- FIXME
      let
        doc' = Document dpre el' dpost
        tl = renderText encSetts doc'
      pure $ TL.toStrict tl -- this will suck, performance-wise

-- | all NodeContent's
nodeContents :: Applicative f => (T.Text -> f T.Text) -> Element -> f Element
nodeContents = elemNodes . nodeContent

-- elemNodes :: Traversal' Element Node
elemNodes :: Applicative f =>
             (Node -> f Node) -> Element -> f Element
elemNodes f (Element en ea ens) = Element <$> pure en <*> pure ea <*> traverse f ens

-- nodeContent :: Traversal' Node T.Text
nodeContent :: Applicative f =>
               (T.Text -> f T.Text) -> Node -> f Node
nodeContent f = \case
  NodeContent t ->
    if
      T.all isSpace t
    then pure $ NodeContent "" -- get rid of whitespace
    else NodeContent <$> f t
  x -> pure x


parsePattern :: T.Text -> Either ParseE FilePath
parsePattern = parse stachePatternP ""

stachePatternP :: Parser FilePath
stachePatternP = stache htmlP

-- | parse only ".html" filenames
htmlP :: Parser FilePath
htmlP = filename "html"

filename :: String -> Parser FilePath
filename ext = do
  spaceConsumer
  fname <- some $ satisfy (/= '.')
  void $ symbol "."
  void $ symbol $ T.pack ext
  pure $ fname <> "." <> ext

stache :: Parser a -> Parser a
stache p = spaceConsumer *> between (symbol "{{") (symbol "}}") p


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



--

-- squareBkts :: Parser a -> Parser a
-- squareBkts p = spaceConsumer *> between (symbol "[") (symbol "]") p
