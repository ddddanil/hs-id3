module Data.ID3.V1.Tag where

import Control.Lens
import Control.Lens.TH
import Data.Generics.Product.Fields
import Data.Text.Prettyprint.Doc

import Data.ID3.Genre


-- v1.0 and v1.1

data ID3v1xTag = ID3v1xTag
  { title :: !Text
  , artist :: !Text
  , album :: !Text
  , year :: !Text
  , comment :: !Text
  , track :: !(Maybe Word8)
  , genre :: !Genre
  }
  deriving (Eq, Show, Generic)

isv11 :: ID3v1xTag -> Bool
isv11 = has $ field @"track" . _Just

-- TODO instance pretty tag


-- Enhanced tag

data ID3v1ETag = ID3v1ETag
  { title :: !Text
  , artist :: !Text
  , album :: !Text
  , speed :: !Word8
  , genre :: !Text
  , startTime :: !(Int, Int)
  , endTime :: !(Int, Int)
  -- From 1.1
  , year :: !Text
  , comment :: !Text
  , track :: !(Maybe Word8)
  }
  deriving (Eq, Show, Generic)


-- v1.2 tag

data ID3v12Tag = ID3v12Tag
  { title :: !Text
  , artist :: !Text
  , album :: !Text
  , comment :: !Text
  , subgenre :: !Text
  -- From 1.1
  , year :: !Text
  , track :: !(Maybe Word8)
  , genre :: !Genre
  }
  deriving (Eq, Show, Generic)

data ID3v1
  = ID3v1x !ID3v1xTag
  | ID3v1E !ID3v1ETag
  | ID3v12 !ID3v12Tag
  deriving (Eq, Show, Generic)
