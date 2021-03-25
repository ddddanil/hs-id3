module Data.ID3.Tag where

import Control.Lens.Getter
import Control.Lens.TH
import Data.Text.Prettyprint.Doc

import Data.ID3.Genre

-- | ID3 Supported versions
data ID3Ver
  = ID3v1Ver { is_v11 :: Bool, is_Enh :: Bool, is_v12 :: Bool }
  | ID3v2Ver { major :: Int, minor :: Int }


-- v1.0 and v1.1

data ID3v1xTag = ID3v1xTag
  { _iD3v1xTagTitle :: LText
  , _iD3v1xTagArtist :: LText
  , _iD3v1xTagAlbum :: LText
  , _iD3v1xTagYear :: LText
  , _iD3v1xTagComment :: LText
  , _iD3v1xTagTrack :: Maybe Word8
  , _iD3v1xTagGenre :: Genre
  }
  deriving (Eq, Show)
makeFields ''ID3v1xTag

isv11 :: ID3v1xTag -> Bool
isv11 tag = isJust (tag^.track)

-- TODO instance pretty tag


-- Enhanced tag

data ID3v1ETag = ID3v1ETag
  { _iD3v1ETagTitle :: LText
  , _iD3v1ETagArtist :: LText
  , _iD3v1ETagAlbum :: LText
  , _iD3v1ETagSpeed :: Word8
  , _iD3v1ETagGenre :: LText
  , _iD3v1ETagStart_time :: (Int, Int)
  , _iD3v1ETagEnd_time :: (Int, Int)
  -- From 1.1
  , _iD3v1ETagYear :: LText
  , _iD3v1ETagComment :: LText
  , _iD3v1ETagTrack :: Maybe Word8
  }
  deriving (Eq, Show)
makeFields ''ID3v1ETag


-- v1.2 tag

data ID3v12Tag = ID3v12Tag
  { _iD3v12TagTitle :: LText
  , _iD3v12TagArtist :: LText
  , _iD3v12TagAlbum :: LText
  , _iD3v12TagComment :: LText
  , _iD3v12TagSubgenre :: LText
  -- From 1.1
  , _iD3v12TagYear :: LText
  , _iD3v12TagTrack :: Maybe Word8
  , _iD3v12TagGenre :: Genre -- Do custom Type
  }
  deriving (Eq, Show)
makeFields ''ID3v12Tag

data ID3v1
  = ID3v1x ID3v1xTag
  | ID3v1E ID3v1ETag
  | ID3v12 ID3v12Tag
  deriving (Eq, Show)
makePrisms ''ID3v1
