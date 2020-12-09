{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import CLI (cli, Command(..))
import CLI.Init (cliInit)

main :: IO ()
main = do
  comm <- cli
  case comm of
    Init fp -> cliInit fp
    Build fp -> undefined
