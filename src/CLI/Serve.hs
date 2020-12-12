{-# options_ghc -Wno-unused-imports #-}
module CLI.Serve (cliServe) where

import Control.Concurrent (threadDelay)
import Control.Monad (void, forever)
-- wai=extra
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
-- wai-middleware-static
import Network.Wai.Middleware.Static (static, Policy, staticPolicy, only, tryPolicy)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory)
-- fsnotify
import System.FSNotify (Event(..), withManager, watchDir)
-- scotty
import Web.Scotty (scotty, scottyOpts, Options(..), get, middleware)
-- warp
import Network.Wai.Handler.Warp (Port)

cliServe :: Port -> FilePath -> IO ()
cliServe p fp = do
  fpAbs <- makeAbsolute fp
  let dir = takeDirectory fpAbs
  withManager $ \mgr -> do
    void $ watchDir mgr dir (\e -> case e of {Modified{} -> True; _ -> False} ) $ (\ _ -> serveFile p fp)
    forever $ threadDelay 1000000

serveFile :: Port -> FilePath -> IO ()
serveFile p fp = do
    putStrLn $ unwords ["twelve : serving", fp ]
    scotty  p $ do
      -- middleware static
      -- middleware logStdoutDev
      middleware $ staticPolicy (only [
                                    ("", fp)
                                    ])


-- main =
--   withManager $ \mgr -> do
--     -- start a watching job (in the background)
--     watchDir
--       mgr          -- manager
--       "."          -- directory to watch
--       (const True) -- predicate
--       print        -- action

--     -- sleep forever (until interrupted)
--     forever $ threadDelay 1000000
