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
        putStrLn "twelve : found pre-existing config file"
        case cfge of
          Left e -> error e
          Right (CD din dout) -> do
            createDirectoryIfMissing True din
            createDirectoryIfMissing True dout
    else
      do
        writeConfig cfg "twelve.json"
        putStrLn "twelve : written config file"
        createDirectoryIfMissing True fpin
        createDirectoryIfMissing True fpout



