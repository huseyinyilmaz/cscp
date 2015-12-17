{-# LANGUAGE OverloadedStrings #-}

module Copy where

import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import System.FSNotify
import Turtle

data SyncInfo = SyncInfo {source::Text, destination::Text} deriving (Show)

watch :: (SyncInfo -> IO()) -> SyncInfo -> IO ()
watch f s = do
  withManager $ \mgr -> do
    putStrLn $ "SyncInfo: " ++  (show s)
    -- start a watching job (in the background)
    watchTree
      mgr          -- manager
      ((T.unpack.source) s)          -- directory to watch
      (const True) -- predicate
      (\_->f s)        -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000

rsync :: SyncInfo -> Shell Text
rsync syncInfo = do
  (echo . T.pack . show) syncInfo
  inproc "rsync" ["-arPvz", "--exclude", ".git", "--exclude","*.pyc",
                  (source syncInfo), (destination syncInfo)] empty
