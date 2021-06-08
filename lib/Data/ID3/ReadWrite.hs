module Data.ID3.ReadWrite where

import Data.ByteString.Builder
import Data.ID3.Parse

class ReadWrite a where
  parse :: Parser a
  write :: a -> Builder

