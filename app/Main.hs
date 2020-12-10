{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import CLI (cli, Command(..))
import CLI.Init (cliInit)
import CLI.Build (cliBuild)

main :: IO ()
main = do
  comm <- cli
  case comm of
    Init cfg -> cliInit cfg
    Build cfg fpin -> cliBuild cfg fpin
