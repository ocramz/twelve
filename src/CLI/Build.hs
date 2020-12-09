{-# OPTIONS_GHC -Wno-unused-imports #-}
module CLI.Build where

-- directory
import System.Directory (makeAbsolute, listDirectory)
-- filepath
import System.FilePath.Posix (takeExtension, (</>))

-- import Text.Html (parsePattern)

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
