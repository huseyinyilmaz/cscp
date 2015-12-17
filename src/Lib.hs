{-# LANGUAGE OverloadedStrings #-}
module Lib (run) where

import Turtle
import Copy(rsync)
import qualified Data.Text as T

parser :: Parser (Text, Text)
parser = (,) <$> (argText "source" "source directory to copy")
             <*> argText "destination" "destination directory to copy"

run :: IO ()
run = do
  (src, dst) <- options
    "Continuesly copies source directory to given destinatio directory"
    parser
  view $ rsync src dst
  print $ T.append "Destination: " dst
  print $ T.append "Source: " src
