module Data.ID3.V2.Header where

import qualified Data.ByteString as BS
import Data.ByteString.Lens
import Data.Binary
import Data.Bits
import Data.Bits.Lens
import Control.Lens
import Text.Megaparsec
import qualified Data.ByteString.Builder as B
import Data.ID3.Version
import Data.ID3.Parse
import Data.ID3.Build
import Data.ID3.ReadWrite


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
  , _size    :: !Word32
  }
  deriving (Eq, Show, Generic)
makeLenses ''ID3v2Header

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

asChunks :: Int -> Iso [a] [b] [[a]] [[b]]
asChunks n = iso (chunks n) concat

makeBits :: Bits a => [Bool] -> a
makeBits = ifoldlOf itraversed
    (\i a n -> if n then a .|. bit i else a)
    zeroBits

-- asBits :: (Bits c, Bits d) => Iso c d [Bool] [Bool]
-- asBits = iso (\b -> b^..bits) makeBits

unsynchronised :: (Bits c, Num d, Bits d) => Iso ByteString ByteString c d 
unsynchronised = iso unsynch synch
  where
    synch :: (Num d, Bits d) => d -> ByteString
    synch b = BS.pack (b ^.. bits & asChunks 7 . traversed %~ (:[]) . makeBits . (flip snoc) False)
    unsynch :: Bits c => ByteString -> c
    unsynch bs = bs ^.. bytes . bits . indices (/= 7) & makeBits
    

parsev2Header :: Parser ID3v2Header
parsev2Header = do
  parseString @Text "ID3"
  version <- ID3v2Ver <$> (fromIntegral <$> anySingle) <*> (fromIntegral <$> anySingle)
  flags <- anySingle
  raw_size <- takeP (Just "Byte") 4
  let size = raw_size ^. unsynchronised
  return $ ID3v2Header version flags size

writev2Header :: ID3v2Header -> B.Builder
writev2Header h = 
  putSText "ID3"
  <> B.word8 (h^.version.v2minor.enum)
  <> B.word8 (h^.version.v2revision.enum)
  <> B.word8 (h^.flags)
  <> B.word32BE (decode $ h^.size.from unsynchronised)

instance ReadWrite ID3v2Header where
  parse = parsev2Header
  write = writev2Header
