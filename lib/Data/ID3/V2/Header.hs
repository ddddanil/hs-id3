module Data.ID3.V2.Header where

import Data.ID3.Version

data ID3v2HeaderFlags
  = Unsynchronisation
  | Extended
  | Experimental
  | Footer
  deriving(Eq, Show, Generic, Enum)

data ID3v2Header = ID3v2Header
  { _version :: !Word16
  , _flags   :: !Word32
  , _size    :: !Word32
  }
  deriving(Eq, Show, Generic)


