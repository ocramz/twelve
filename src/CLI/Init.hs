module CLI.Init (cliInit) where

-- directory
import System.Directory (createDirectoryIfMissing)

cliInit :: FilePath -> IO ()
cliInit = createDirectoryIfMissing True
