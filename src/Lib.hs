{-# LANGUAGE OverloadedStrings #-}
module Lib (run) where

import Turtle
import qualified Data.Text as Text
import Copy--(rsync, SyncInfo)

parser :: Parser (Text, Text, [Text])
parser = (,,) <$> (argText "source" "source directory to copy")
              <*> (argText "destination" "destination directory to copy")
              <*> (many (optText "exclude" 'e' "Exclude Following Paths(Coma separated List)"))

run :: IO ()
run = do
  (src, dst, exs) <- options
    "Continuesly copies source directory to given destinatio directory"
    parser
  let syncInfo = SyncInfo{source=src,
                          destination=dst,
                          excludes=exs}
  view $ rsync syncInfo
  watch (view.rsync) syncInfo
