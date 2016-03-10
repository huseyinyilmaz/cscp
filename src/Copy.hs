{-# LANGUAGE OverloadedStrings #-}

module Copy where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newMVar, tryTakeMVar, putMVar)
import Data.Maybe (isJust)
import qualified Data.Text as T
import System.FSNotify
import Turtle


data SyncInfo = SyncInfo {source::Text, destination::Text} deriving (Show)


watch :: (SyncInfo -> IO()) -> SyncInfo -> IO ()
watch fun syncInfo = do
  lock <- newMVar ()
  withManager $ \mgr -> do
    putStrLn $ "SyncInfo: " ++  (show syncInfo)
    -- start a watching job (in the background)
    void $ watchTree
      mgr          -- manager
      ((T.unpack . source) syncInfo)          -- directory to watch
      (const True) -- predicate
      (\_-> void $ forkIO $ do
          took <- tryTakeMVar lock
          when (isJust took) $ do
            putStrLn "Change detected"
            threadDelay (1 * 1000 * 1000)
            void $ fun syncInfo -- action
            putMVar lock ())
    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000

--rsync :: SyncInfo -> Shell Text
rsync syncInfo =
  inproc "rsync" ["-arPvz", "--exclude", ".git", "--exclude", "*.pyc", "--delete",
                  (source syncInfo), (destination syncInfo)] empty
