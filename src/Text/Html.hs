{-# LANGUAGE OverloadedStrings #-}
{-# language LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Text.Html where

import Control.Applicative (Alternative (..))
import Control.Applicative.Combinators (between)
import Control.Monad (void)
import Data.Char (isSpace)

import Data.Void

-- megaparsec
import Text.Megaparsec ( Parsec, ParsecT, parse, parseTest, runParserT, satisfy)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, scientific, skipBlockComment, skipLineComment, space, symbol)
import Text.Megaparsec.Error (ParseErrorBundle (..), errorBundlePretty)

-- directory
import System.Directory (makeAbsolute)
-- filepath
import System.FilePath.Posix (takeExtension, (</>))

-- text
import qualified Data.Text as T (Text, all, pack, unpack)
import qualified Data.Text.IO as T (putStrLn)
import qualified Data.Text.Lazy as TL (Text, toStrict, fromStrict)
import qualified Data.Text.Lazy.IO as TL (readFile, putStrLn)

import Text.XML (ParseSettings, Document(..), Element(..), Node(..), parseText, def, renderText, psDecodeEntities, decodeHtmlEntities, rsPretty, rsXMLDeclaration)
import Data.XML.Types (Name(..))


import Config (Config(..))

import Prelude hiding (readFile)


-- top level
loadAndProcess :: Foldable t =>
                  Config -> t FilePath -> FilePath -> IO TL.Text
loadAndProcess cfg inPaths fp = do
  (Document dpre el dpost) <- loadDoc cfg fp
  let
    decSetts = def { psDecodeEntities = decodeHtmlEntities }
    encSetts = def { rsPretty = True, rsXMLDeclaration = False }
  el' <- expand cfg inPaths decSetts el
  let doc' =  Document dpre el' dpost
  pure $ renderText encSetts doc'

-- | expand the element by dereferencing its internal pointers
expand :: Foldable t =>
          Config -> t FilePath -> ParseSettings -> Element -> IO Element
expand cfg@(CD dirIn _) inPaths psetts el = expandElement el $ \t ->
  case parseMatch psetts (TL.fromStrict t) of
    MatchText tl -> pure $ NodeContent $ TL.toStrict tl
    MatchRef fpIn -> do
      fpInAbs <- makeAbsolute (dirIn </> fpIn)
      if fpInAbs `elem` inPaths
        then NodeElement <$> loadElementH cfg inPaths fpInAbs
        else error $ unwords [fpInAbs, "is not present in the input directory"]
    MatchDoc (Document _ elInner _) ->
      NodeElement <$> expand cfg inPaths psetts elInner


-- | Load an Element from a file path
loadElementH :: Foldable t =>
                Config -> t FilePath -> FilePath -> IO Element
loadElementH cfg inPaths fp = do
  (Document _ el _) <- loadDoc cfg fp
  let
    decSetts = def { psDecodeEntities = decodeHtmlEntities }
  expand cfg inPaths decSetts el

loadDoc :: Config -> FilePath -> IO Document
loadDoc (CD dirIn _) fp = do
  let fpRel = dirIn </> fp
  tl0 <- TL.readFile fpRel
  let
    decSetts = def { psDecodeEntities = decodeHtmlEntities }
  case parseText decSetts tl0 of
    Right hdoc -> pure hdoc
    Left e -> error $ unwords ["Error while attempting to parse", fp, ":", show e]


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

expandNode :: Applicative f =>
              (T.Text -> f Node)  -- ^ references are expanded into ASTs
           -> Node -> f Node
expandNode f = \case
  NodeElement (Element n a ns) ->
    NodeElement <$> (Element <$> pure n <*> pure a <*> traverse (expandNode f) ns)
  NodeContent t ->
    if T.all isSpace t
    then pure $ NodeContent "" -- get rid of whitespace
    else f t
  x -> pure x

expandElement :: Applicative f =>
                 Element -> (T.Text -> f Node) -> f Element
expandElement (Element en ea ens) f =
  Element <$> pure en <*> pure ea <*> traverse (expandNode f) ens


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

type ParseE = ParseErrorBundle T.Text Void




-- -- playground

-- doc, doc2 :: LBS.ByteString
-- doc = "<html> <body>{{ card2.html }}</body></html>"

-- doc2 = "<!DOCTYPE html><html></html>"

-- docParse :: Document
-- docParse =
--   Document
--     (Prologue [] Nothing [])
--     (Element (Name "html" Nothing Nothing) mempty [
--         NodeContent " ",
--         NodeElement (Element (Name "body" Nothing Nothing) mempty [NodeContent "{{ card2.html }}"])
--         ]
--       ) []
