module CLI.Init (cliInit) where

-- directory
import System.Directory (createDirectoryIfMissing)

import CLI

cliInit :: FilePath -> IO ()
cliInit = createDirectoryIfMissing True
