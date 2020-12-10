{-# OPTIONS_GHC -Wno-unused-imports #-}
module CLI (cli, Command(..)) where

import Control.Applicative (Alternative (..), some)
import Options.Applicative (Parser, command, customExecParser, execParser, flag', fullDesc, header, help, helper, info, long, metavar, prefs, progDesc, short, showDefault, showHelpOnEmpty, strOption, subparser, switch, value, (<**>))

import Config (Config(..))

{- commands

* init:
  - check if config file is present
  - CLI : input dir for templates
  - CLI : output dir
  - write config file

* build :
  - read config (input, output dir)
  - CLI : input filepath(s)
  - assemble result(s)
  - write file(s)

-}

cli :: IO Command
cli = customExecParser p opts
  where
    p = prefs showHelpOnEmpty
    opts = info (commandP <**> helper) (fullDesc <> progDesc desc <> header "twelve, a little static website build tool")
    desc = "twelve lets you build an HTML page from a collection of templates. Please refer to the README for details.\n github.com/ocramz/twelve"

data Command =
  Init Config
  | Build FilePath

commandP :: Parser Command
commandP = subparser (
  command "init" (info initP (progDesc "Initialize a 'twelve' project")) <>
  command "build" (info buildP (progDesc "Build an HTML page"))
         )
  where
    initP = Init <$> configP
    buildP = Build <$> cliFileP


cliFileP :: Parser FilePath
cliFileP = 
  strOption (
    short 'f' <> metavar "FILEPATH" <> help "path of input file"
            )

configP :: Parser Config
configP = CD <$> inDirP <*> outDirP

inDirP, outDirP :: Parser FilePath
inDirP = strOption (short 'i' <> long "dir-in" <> value "_templates" <> showDefault <> metavar "DIR" <> help "input directory for HTML templates")
outDirP = strOption (short 'o' <> long "dir-out" <> value "_site" <> showDefault <> metavar "DIR" <>help "output directory")
