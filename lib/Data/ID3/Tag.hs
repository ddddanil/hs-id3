module Data.ID3.Tag where

import Protolude
import qualified Data.Text as T
import Control.Lens.TH

-- | ID3 Supported versions
data ID3Ver
  = ID3v1  -- ^ v1 and v1.1
  | ID3v1E -- ^ unofficial Enhanced TAG
  | ID3v12 -- ^ v1.2
  | ID3v22 -- ^ v2.2
  | ID3v23 -- ^ v2.3
  | ID3v24 -- ^ v2.4


-- v1.0 and v1.1

data ID3v1Tag = ID3v1Tag
  { _title :: T.Text
  , _artist :: T.Text 
  , _album :: T.Text
  , _year :: T.Text
  , _comment :: T.Text
  , _track :: Maybe Word8
  , _genre :: Word8 -- Do custom Type
  }
  deriving (Eq, Show)
makeFields ''ID3v1Tag


-- Enhanced tag

data ID3v1ETag = ID3v1ETag
  { _title :: T.Text
  , _artist :: T.Text
  , _album :: T.Text
  , _speed :: Word8
  , _genre :: T.Text
  , _start_time :: (Int, Int)
  , _end_time :: (Int, Int)
  }
  deriving (Eq, Show)
makeFields ''ID3v1ETag
