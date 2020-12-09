{-# OPTIONS_GHC -Wno-unused-imports #-}
module CLI.Build where

-- directory
import System.Directory (makeAbsolute, listDirectory)
-- filepath
import System.FilePath.Posix (takeExtension)

-- import CLI

paths :: FilePath -> IO [FilePath]
paths fp = do
  fpnorm <- makeAbsolute fp
  listDirectory fpnorm


-- loadModelFromDir dp = do
--   dpnorm <- makeAbsolute dp
--   fps <- listDirectory dpnorm
--   let fpaths = map (dp </>) $ filter (\fp -> takeExtension fp == ".yml") fps
--   loadModel fpaths
