{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module CLI.Serve (cliServe) where

import Control.Concurrent (threadDelay)
import Control.Monad (void, forever)
-- wai=extra
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
-- wai-middleware-static
import Network.Wai.Middleware.Static (static, Policy, staticPolicy, only, tryPolicy)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory)
-- containers
import qualified Data.Map as M (Map, fromList)
-- scotty
import Web.Scotty (scotty, scottyOpts, Options(..), get, middleware, html)
-- text
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.IO as TL (readFile)
import Data.Text.IO (readFile)
-- warp
import Network.Wai.Handler.Warp (Port)

import Text.XML (ParseSettings, Document(..), Element(..), Node(..), parseText, def, renderText, psDecodeEntities, decodeHtmlEntities, rsPretty, rsXMLDeclaration)
import Data.XML.Types (Name(..))

import Text.Html (loadDoc)


-- liveJs :: TL.Text
-- liveJs = "<script type=\"text/javascript\" src=\"http://livejs.com/live.js\"></script>"

liveJsNode :: Node
liveJsNode = NodeElement $ Element n attrs []
  where
    n = Name "script" Nothing Nothing
    attrs = M.fromList [
      (Name "src" Nothing Nothing, "http://livejs.com/live.js"),
      (Name "type" Nothing Nothing, "text/javascript")
      ]

modifyHeadNode :: ([Node] -> [Node]) -> Element -> Element
modifyHeadNode f (Element n a ns)
  | n == "head" = Element n a (f ns)
  | otherwise = Element n a $ map goNodes ns
  where
    goNodes = \case
      NodeElement e -> NodeElement $ modifyHeadNode f e
      x -> x

addLiveJs :: Document -> Document
addLiveJs (Document dpre el dpost) = Document dpre el' dpost
  where
    el' = modifyHeadNode (liveJsNode :) el

-- | Load, parse an HTML file and add the live.js annotation in its HEAD tag
injectLiveJs :: FilePath -> IO TL.Text
injectLiveJs fp = do
  let encSetts = def { rsPretty = True, rsXMLDeclaration = False }
  doc' <- addLiveJs <$> loadDoc fp
  pure $ renderText encSetts doc'

cliServe :: Port -> FilePath -> IO ()
cliServe p fp = do
  tl' <- injectLiveJs fp
  putStrLn $ unwords ["twelve : serving", fp ]
  scotty  p $ do
    get "/" $ html tl'


