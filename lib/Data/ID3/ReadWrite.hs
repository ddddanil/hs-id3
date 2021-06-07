module Data.ID3.ReadWrite where

import qualified Data.ByteString.Builder as B
import Data.ID3.Parse

class ReadWrite a where
  parse :: Parser a
  write :: a -> B.Builder
