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
import GHC.Exception (SomeException)

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
-- directory
import System.Directory (makeAbsolute)
-- filepath
import System.FilePath.Posix (takeExtension, (</>))
-- lens
-- import Control.Lens (Traversal', Prism, (^?), (^..), (...), over, (.~))
-- text
import qualified Data.Text as T (Text, all, pack, unpack)
import qualified Data.Text.IO as T (putStrLn)
import qualified Data.Text.Lazy as TL (Text, toStrict, fromStrict)
import qualified Data.Text.Lazy.IO as TL (readFile)

import Text.XML (ParseSettings, Document(..), Prologue(..), Element(..), Node(..), readFile, parseText, parseLBS, def, renderText, psDecodeEntities, decodeHtmlEntities, rsPretty, rsXMLDeclaration)
import Data.XML.Types (Name(..))
-- import Text.Xml.Lens (AsHtmlDocument(..), _HtmlDocument, html, name, text, texts)
-- import Text.Xml.Lens.LowLevel (_NodeContent)

import Config (Config(..))

import Prelude hiding (readFile)



loadAndProcess :: Config
               -> [FilePath] -- ^ absolute paths of HTML files in input dir
               -> FilePath -- ^ template file to be processed
               -> IO TL.Text
loadAndProcess cfg@(CD dirIn _) inPaths fp = do
  let fpRel = dirIn </> fp
  tl0 <- TL.readFile fpRel
  flatten cfg inPaths tl0

flatten :: Config -> [FilePath] -> TL.Text -> IO TL.Text
flatten cfg@(CD dirIn _) inPaths tl =
  let
    decSetts = def { psDecodeEntities = decodeHtmlEntities }
    encSetts = def { rsPretty = True, rsXMLDeclaration = False }
  in
    case parseMatch decSetts tl of
      MatchText tl' -> pure tl'
      -- MatchDoc doc' -> pure $ renderText encSetts doc' -- DEBUG
      MatchDoc (Document dpre el dpost) -> do
        el' <- nodeContents el $ \t ->
          TL.toStrict <$> flatten cfg inPaths (TL.fromStrict t)
        let
          doc' = Document dpre el' dpost
          tl' = renderText encSetts doc'
        pure tl'
      MatchRef fpIn -> do
        fpInAbs <- makeAbsolute (dirIn </> fpIn)
        if fpInAbs `elem` inPaths
          then loadAndProcess cfg inPaths fpInAbs
          else error $ unwords [fpInAbs, "does not occur in", unwords inPaths ]


data Match =
    MatchRef FilePath -- ^ node content is a reference to another template
  | MatchDoc Document -- ^ " contains an XHTML document
  | MatchText TL.Text -- ^ free text
  deriving (Eq, Show)

parseMatch :: ParseSettings -- ^ settings to the XHTML parser
           -> TL.Text -- ^ text to be parsed
           -> Match
parseMatch opts tl = case parseText opts tl of
  Left _ -> case parse stachePatternP "" (TL.toStrict tl) of
    Left _ -> MatchText tl
    Right fp -> MatchRef fp
  Right hdoc -> MatchDoc hdoc


-- | all NodeContent's
nodeContents :: Applicative f =>
         Element -> (T.Text -> f T.Text) -> f Element
nodeContents (Element en ea ens) f =
  Element <$> pure en <*> pure ea <*> traverse goNodes ens
  where
    goNodes = \case
      NodeElement (Element n a ns) ->
        NodeElement <$> (Element <$> pure n <*> pure a <*> traverse goNodes ns)
      NodeContent t ->
        if T.all isSpace t
        then pure $ NodeContent "" -- get rid of whitespace
        else NodeContent <$> f t
      x -> pure x

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

hits :: IO Element
hits = let (Document _ el _) = docParse in nodeContents el $ \t ->
  case parsePattern t of
    Right _ -> pure $ T.pack "!!!"
    Left e -> error $ errorBundlePretty e
