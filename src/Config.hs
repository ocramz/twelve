{-# language DeriveGeneric #-}
{-# options_ghc -Wno-unused-imports #-}
module Config (Config(..), readConfig, writeConfig) where

import GHC.Generics (Generic(..))

-- aeson
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode', encode)
-- bytestring
import Data.ByteString.Lazy (ByteString, readFile, writeFile)

import Prelude hiding (readFile, writeFile)

readConfig :: FilePath -> IO (Either String Config)
readConfig fp = do
  bs <- readFile fp
  pure $ eitherDecode' bs

writeConfig :: Config -> FilePath -> IO ()
writeConfig cd fp = writeFile fp $ encode cd

data Config =
  CD {
      inputDir :: FilePath
    , outputDir :: FilePath
     } deriving (Generic)

instance FromJSON Config
instance ToJSON Config
