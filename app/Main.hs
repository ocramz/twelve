{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import Data.Foldable (traverse_)

import CLI (cli, Command(..))
import CLI.Init (cliInit)
import CLI.Build (cliBuild)
import CLI.Serve (cliServe)
import Config (Config, readConfig)

main :: IO ()
main = do
  comm <- cli
  case comm of
    Init cfg -> cliInit cfg
    Serve p fp -> cliServe p fp
    Build cm fpsin -> do
      case cm of
        Just cfg -> atLeast1 cfg fpsin
        Nothing -> do
          cfge <- readConfig "twelve.json"
          case cfge of
            Right cfg -> atLeast1 cfg fpsin
            Left e -> error e

-- | build at least one error file or error out
atLeast1 :: Foldable t => Config -> t FilePath -> IO ()
atLeast1 c fs
  | null fs = error "please provide at least one input file"
  | otherwise = traverse_ (cliBuild c) fs
