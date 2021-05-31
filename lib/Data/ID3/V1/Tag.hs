module Data.ID3.V1.Tag where

import Control.Lens.Getter
import Control.Lens.TH
import Data.Generics.Product.Fields
import Data.Text.Prettyprint.Doc

import Data.ID3.Genre

-- | ID3 Supported versions
data ID3Ver
  = ID3v1Ver { is_v11 :: Bool, is_Enh :: Bool, is_v12 :: Bool }
  | ID3v2Ver { major :: Int, minor :: Int }


-- v1.0 and v1.1

data ID3v1xTag = ID3v1xTag
  { title :: LText
  , artist :: LText
  , album :: LText
  , year :: LText
  , comment :: LText
  , track :: Maybe Word8
  , genre :: Genre
  }
  deriving (Eq, Show, Generic)

isv11 :: ID3v1xTag -> Bool
isv11 tag = isJust (tag ^. field @"track")

-- TODO instance pretty tag


-- Enhanced tag

data ID3v1ETag = ID3v1ETag
  { title :: LText
  , artist :: LText
  , album :: LText
  , speed :: Word8
  , genre :: LText
  , startTime :: (Int, Int)
  , endTime :: (Int, Int)
  -- From 1.1
  , year :: LText
  , comment :: LText
  , track :: Maybe Word8
  }
  deriving (Eq, Show, Generic)


-- v1.2 tag

data ID3v12Tag = ID3v12Tag
  { title :: LText
  , artist :: LText
  , album :: LText
  , comment :: LText
  , subgenre :: LText
  -- From 1.1
  , year :: LText
  , track :: Maybe Word8
  , genre :: Genre -- Do custom Type
  }
  deriving (Eq, Show, Generic)

data ID3v1
  = ID3v1x ID3v1xTag
  | ID3v1E ID3v1ETag
  | ID3v12 ID3v12Tag
  deriving (Eq, Show, Generic)
