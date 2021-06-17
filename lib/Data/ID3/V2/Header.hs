module Data.ID3.V2.Header where

import qualified Data.ByteString as BS
import Data.ByteString.Lens
import Data.Binary
import Data.Bits
import Data.Bits.Lens
import Control.Lens
import Text.Megaparsec hiding (parse)
import Data.ByteString.Builder
import Data.ID3.Version
import Data.ID3.Parse
import Data.ID3.Build
import Data.ID3.ReadWrite
import Data.ID3.V2.Synchronisation


-- Standard header

data ID3v2HeaderFlags
  = Unsynchronisation
  | Extended
  | Experimental
  | Footer
  deriving (Eq, Show, Enum, Generic)

data ID3v2Header = ID3v2Header
  { _version :: !ID3v2Ver
  , _flags   :: !Word8
  , _size    :: !SynchInt
  }
  deriving (Eq, Show, Generic)
makeLenses ''ID3v2Header

parsev2Header :: Parser ID3v2Header
parsev2Header = do
  parseString "ID3"
  version <- ID3v2Ver <$> (fromIntegral <$> anySingle) <*> (fromIntegral <$> anySingle)
  flags <- anySingle
  size <- parse @SynchInt
  return $ ID3v2Header version flags size

writev2Header :: ID3v2Header -> Builder
writev2Header h = 
  writeText "ID3"
  <> word8 (h^.version.v2minor.enum)
  <> word8 (h^.version.v2revision.enum)
  <> word8 (h^.flags)
  <> write (h^.size)

instance ReadWrite ID3v2Header where
  parse = parsev2Header
  write = writev2Header


