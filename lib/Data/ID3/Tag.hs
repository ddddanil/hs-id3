module Data.ID3.Tag where

import Control.Lens.Getter
import Control.Lens.TH
import Data.Text.Prettyprint.Doc

import Data.ID3.Genre

-- | ID3 Supported versions
data ID3Ver
  = ID3v1 { is_v11 :: Bool, is_Enh :: Bool, is_v12 :: Bool }
  | ID3v2 { major :: Int, minor :: Int }


-- v1.0 and v1.1

data ID3v1Tag = ID3v1Tag
  { _iD3v1TagTitle :: Text
  , _iD3v1TagArtist :: Text 
  , _iD3v1TagAlbum :: Text
  , _iD3v1TagYear :: Text
  , _iD3v1TagComment :: Text
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
  { _iD3v1ETagTitle :: Text
  , _iD3v1ETagArtist :: Text
  , _iD3v1ETagAlbum :: Text
  , _iD3v1ETagSpeed :: Word8
  , _iD3v1ETagGenre :: Text
  , _iD3v1ETagStart_time :: (Int, Int)
  , _iD3v1ETagEnd_time :: (Int, Int)
  -- From 1.1
  , _iD3v1ETagYear :: Text
  , _iD3v1ETagComment :: Text
  , _iD3v1ETagTrack :: Maybe Word8
  }
  deriving (Eq, Show)
makeFields ''ID3v1ETag


-- v1.2 tag

data ID3v12Tag = ID3v12Tag
  { _iD3v12TagTitle :: Text
  , _iD3v12TagArtist :: Text
  , _iD3v12TagAlbum :: Text
  , _iD3v12TagComment :: Text
  , _iD3v12TagSubgenre :: Text
  -- From 1.1
  , _iD3v12TagYear :: Text
  , _iD3v12TagTrack :: Maybe Word8
  , _iD3v12TagGenre :: Genre -- Do custom Type
  }
  deriving (Eq, Show)
makeFields ''ID3v12Tag