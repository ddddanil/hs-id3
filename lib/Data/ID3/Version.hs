module Data.ID3.Version (
  ID3Version
, major
, minor
, _ID3v1Version
, _ID3v2Version
, ID3v1Ver
, _ID3v10Ver
, _ID3v11Ver
, _ID3v12Ver
, _ID3v1EVer
, ID3v2Ver
, v2minor
) where

import Control.Lens

data ID3v1Ver
  = ID3v10Ver
  | ID3v11Ver
  | ID3v12Ver
  | ID3v1EVer
  deriving (Eq, Show, Enum, Generic)

newtype ID3v2Ver = ID3v2Ver
  { _v2minor :: Int
  }
  deriving (Eq, Show, Generic)

data ID3Version
  = ID3v1Version ID3v1Ver
  | ID3v2Version ID3v2Ver
  deriving (Eq, Show, Generic)

makePrisms ''ID3v1Ver
makeLenses ''ID3v2Ver
makePrisms ''ID3Version

major :: Getter ID3Version Int
major = to $ \case
  ID3v1Version _ -> 1
  ID3v2Version _ -> 2

minor :: Getter ID3Version Int
minor = to $ \case
  ID3v1Version ver -> case ver of
    ID3v10Ver -> 0
    ID3v11Ver -> 1
    ID3v12Ver -> 2
    ID3v1EVer -> 3
  ID3v2Version (ID3v2Ver minor) -> minor
