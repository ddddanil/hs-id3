module Data.ID3.V2.Synchronisation where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lens
import Data.Binary
import Data.Bits
import Data.Bits.Lens
import Data.Bits.Lens.Extra
import Data.List.Lens.Extra
import Data.Monoid.Extra
import Text.Megaparsec
import GHC.Real.Lens
import Data.ID3.Version
import Data.ID3.Parse
import Data.ID3.Build
import Data.ID3.ReadWrite

newtype SynchInt = SynchInt Word32
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Bits, Num, Real, Enum, Integral)

fromBits :: Bits a => [Bool] -> a
fromBits = ifoldlOf itraversed
    (\i a n -> if n then a .|. bit i else a)
    zeroBits

synchronisedInt :: Iso' SynchInt Word32
synchronisedInt = iso unsynch synch
  where
    synch :: Word32 -> SynchInt
    unsynch x = fold bytes ^. _Wrapping' Disjunction . coerced -- _Wrapping' SynchInt
      where
        bytes :: [Disjunction Word32]
        bytes = x ^.. bytewise . integral @Word32 . _Unwrapping' Disjunction
        fold = ifoldMap (\i a -> shiftL a (i * 7))
    unsynch :: SynchInt -> Word32
    synch x = x & partsOf bytewise .~ bytes & view integral
      where
        bytes :: [Word8]
        bytes = x ^.. bits & asChunks 7 . traversed %~ one . fromBits

parseSynchInt :: Parser SynchInt
parseSynchInt = do
  raw_size <- toLazy <$> takeP (Just "Unsynchronised byte") 4
  return $ decode raw_size ^. from synchronisedInt

writeSynchInt :: SynchInt -> B.Builder
writeSynchInt s = B.word32BE (s ^. synchronisedInt)

instance ReadWrite SynchInt where
  parse = parseSynchInt
  write = writeSynchInt


-- asPairs :: f a -> f (a, a)
-- asPairs =

-- synchronisedBS :: Iso' ByteString ByteString
-- synchronisedBS = iso unsynch synch
--   where
--     unsynch :: ByteString -> ByteString
--     unsynch = 