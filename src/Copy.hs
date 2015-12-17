{-# LANGUAGE OverloadedStrings #-}

module Copy where
import Turtle
import qualified Data.Text as T

rsync :: Text -> Text -> Shell Text
rsync src dst = do
  echo $ T.append "Source: " src
  echo $ T.append "Destination: " dst
  inproc "rsync" ["-arPvz", "--exclude", ".git", "--exclude","*.pyc", src, dst] empty
