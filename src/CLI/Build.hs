{-# OPTIONS_GHC -Wno-unused-imports #-}
module CLI.Build (cliBuild) where

-- directory
import System.Directory (makeAbsolute, listDirectory)
-- filepath
import System.FilePath.Posix (takeExtension, (</>), replaceDirectory)

import Data.Text (Text)
import Data.Text.IO (writeFile)

import Text.Html (loadAndProcess)

import Config (Config(..))

import Prelude hiding (writeFile)

cliBuild :: Config
         -> FilePath -- ^ file to be processed
         -> IO ()
cliBuild (CD din dout) fp = do
  fpsIn <- htmlPaths din
  t <- loadAndProcess fpsIn fp
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
  let fpaths = map (dp </>) $ filter (\fp -> takeExtension fp == fext) fps
  pure fpaths


-- loadModelFromDir dp = do
--   dpnorm <- makeAbsolute dp
--   fps <- listDirectory dpnorm
--   let fpaths = map (dp </>) $ filter (\fp -> takeExtension fp == ".yml") fps
--   loadModel fpaths
