module Data.ID3.V1.Instances where

import Data.ID3.ReadWrite
import Data.ID3.V1.Tag
import Data.ID3.V1.Parse
import Data.ID3.V1.Build

instance ReadWrite ID3v1xTag where
  parse = parseID3v1xTag
  write = putID3v1Tag
