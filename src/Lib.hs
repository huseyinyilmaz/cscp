{-# LANGUAGE OverloadedStrings #-}
module Lib (run) where

import Turtle
import Copy--(rsync, SyncInfo)
import qualified Data.Text as T

parser :: Parser (Text, Text)
parser = (,) <$> (argText "source" "source directory to copy")
             <*> argText "destination" "destination directory to copy"

run :: IO ()
run = do
  (src, dst) <- options
    "Continuesly copies source directory to given destinatio directory"
    parser
  let syncInfo = SyncInfo{source=src, destination=dst}
  view $ rsync syncInfo
  watch (view.rsync) syncInfo
