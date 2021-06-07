module Data.ID3.V2.Header where

import qualified Data.ByteString as BS
import Data.ByteString.Lens
import Data.Binary
import Data.Bits
import Data.Bits.Lens
import Control.Lens
import Text.Megaparsec hiding (parse)
import qualified Data.ByteString.Builder as B
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

reverseBits :: Word8 -> Word8
reverseBits x = fromIntegral x'
   where
      !x' = ((fromIntegral x * 0x0202020202 :: Word64) .&. 0x010884422010) `mod` 1023

reversedBits :: Iso' Word8 Word8
reversedBits = involuted reverseBits

data ID3v2Header = ID3v2Header
  { _version :: !ID3v2Ver
  , _flags   :: !Word8
  , _size    :: !SynchInt
  }
  deriving (Eq, Show, Generic)
makeLenses ''ID3v2Header

parsev2Header :: Parser ID3v2Header
parsev2Header = do
  parseString @Text "ID3"
  version <- ID3v2Ver <$> (fromIntegral <$> anySingle) <*> (fromIntegral <$> anySingle)
  flags <- anySingle
  size <- parse @SynchInt
  return $ ID3v2Header version flags size

writev2Header :: ID3v2Header -> B.Builder
writev2Header h = 
  putSText "ID3"
  <> B.word8 (h^.version.v2minor.enum)
  <> B.word8 (h^.version.v2revision.enum)
  <> B.word8 (h^.flags)
  <> write (h^.size)

instance ReadWrite ID3v2Header where
  parse = parsev2Header
  write = writev2Header
