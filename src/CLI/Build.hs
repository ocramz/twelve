{-# OPTIONS_GHC -Wno-unused-imports #-}
module CLI.Build (cliBuild) where

-- directory
import System.Directory (makeAbsolute, listDirectory)
-- filepath
import System.FilePath.Posix (takeExtension, (</>), replaceDirectory)

import Data.Text (Text)
import qualified Data.Text.Lazy as TL (Text)
import Data.Text.Lazy.IO (readFile, writeFile)

import Text.Html (loadAndProcess)

import Config (Config(..))

import Prelude hiding (readFile, writeFile)


buildTest :: Config -> FilePath -> IO TL.Text
buildTest cfg@(CD din _) fp = do
  fpsIn <- htmlPaths din
  loadAndProcess cfg fpsIn fp

cliBuild :: Config
         -> FilePath -- ^ file to be processed
         -> IO ()
cliBuild cfg@(CD din dout) fp = do
  fpsIn <- htmlPaths din
  t <- loadAndProcess cfg fpsIn fp
  let fpout = replaceDirectory fp dout
  writeFile fpout t

htmlPaths :: FilePath -> IO [FilePath]
htmlPaths = paths ".html"

paths :: String
      -> FilePath -- ^ directory path of template files
      -> IO [FilePath]
paths fext dp = do
  dpnorm <- makeAbsolute dp
  fps <- listDirectory dpnorm
  fpsAbs <- traverse (\f -> makeAbsolute $ dp </> f) fps
  let fpaths = filter (\fp -> takeExtension fp == fext) fpsAbs
  pure fpaths


-- loadModelFromDir dp = do
--   dpnorm <- makeAbsolute dp
--   fps <- listDirectory dpnorm
--   let fpaths = map (dp </>) $ filter (\fp -> takeExtension fp == ".yml") fps
--   loadModel fpaths
