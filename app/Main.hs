{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import GitHash

import CLI (cli, Command(..))
import CLI.Init (cliInit)
import CLI.Build (cliBuild)
import Config (readConfig)

gi :: GitInfo
gi = $$tGitInfoCwd

main :: IO ()
main = do
  let gh = giHash gi
  putStrLn $ unwords ["git hash :", gh]  
  comm <- cli
  case comm of
    Init cfg -> cliInit cfg
    Build cm fpin -> do
      case cm of
        Just cfg -> cliBuild cfg fpin
        Nothing -> do
          cfge <- readConfig "twelve.json"
          case cfge of
            Right cfg -> cliBuild cfg fpin
            Left e -> error e
