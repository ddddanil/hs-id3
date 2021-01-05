module Data.ID3.Tag where

import Protolude
import qualified Data.Text as T
import Control.Lens.Getter
import Control.Lens.TH
import Data.Text.Prettyprint.Doc

import Data.ID3.Genre

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
  { _iD3v1TagTitle :: T.Text
  , _iD3v1TagArtist :: T.Text 
  , _iD3v1TagAlbum :: T.Text
  , _iD3v1TagYear :: T.Text
  , _iD3v1TagComment :: T.Text
  , _iD3v1TagTrack :: Maybe Word8
  , _iD3v1TagGenre :: Genre -- Do custom Type
  }
  deriving (Eq, Show)
makeFields ''ID3v1Tag

isv11 :: ID3v1Tag -> Bool
isv11 tag = isJust (tag^.track)

-- TODO instance pretty tag


-- Enhanced tag

data ID3v1ETag = ID3v1ETag
  { _iD3v1ETagTitle :: T.Text
  , _iD3v1ETagArtist :: T.Text
  , _iD3v1ETagAlbum :: T.Text
  , _iD3v1ETagSpeed :: Word8
  , _iD3v1ETagGenre :: T.Text
  , _iD3v1ETagStart_time :: (Int, Int)
  , _iD3v1ETagEnd_time :: (Int, Int)
  -- From 1.1
  , _iD3v1ETagYear :: T.Text
  , _iD3v1ETagComment :: T.Text
  , _iD3v1ETagTrack :: Maybe Word8
  }
  deriving (Eq, Show)
makeFields ''ID3v1ETag


-- v1.2 tag

data ID3v12Tag = ID3v12Tag
  { _iD3v12TagTitle :: T.Text
  , _iD3v12TagArtist :: T.Text
  , _iD3v12TagAlbum :: T.Text
  , _iD3v12TagComment :: T.Text
  , _iD3v12TagSubgenre :: T.Text
  -- From 1.1
  , _iD3v12TagYear :: T.Text
  , _iD3v12TagTrack :: Maybe Word8
  , _iD3v12TagGenre :: Genre -- Do custom Type
  }
  deriving (Eq, Show)
makeFields ''ID3v12Tag