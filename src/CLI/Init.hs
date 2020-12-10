module CLI.Init (cliInit) where

-- directory
import System.Directory (createDirectoryIfMissing, doesFileExist)
-- filepath
import System.FilePath.Posix ()

import Config (Config(..), readConfig, writeConfig)

cliInit :: Config -> IO ()
cliInit cfg@(CD fpin fpout) = do
  configExists <- doesFileExist "twelve.json"
  if configExists
    then
      do
        cfge <- readConfig "twelve.json"
        case cfge of
          Left e -> error e
          Right (CD din dout) -> do
            createDirectoryIfMissing True din
            createDirectoryIfMissing True dout
    else
      do
        writeConfig cfg "twelve.json"
        createDirectoryIfMissing True fpin
        createDirectoryIfMissing True fpout



