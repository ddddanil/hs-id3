module Data.ID3.V2.Synchronisation where

import Data.Monoid.Extra
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

newtype SynchInt = SynchInt Word32
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Bits, Num, Real, Enum, Integral)

shiftedL :: (Bits a, Bits b) => Int -> Iso a b a b
shiftedL n = iso (`shiftL` n) (`shiftR` n)


shiftedR :: (Bits a, Bits b) => Int -> Iso a b a b
shiftedR n = iso (`shiftR` n) (`shiftL` n)

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

asChunks :: Int -> Iso [a] [b] [[a]] [[b]]
asChunks n = iso (chunks n) concat

fromBits :: Bits a => [Bool] -> a
fromBits = ifoldlOf itraversed
    (\i a n -> if n then a .|. bit i else a)
    zeroBits

integral :: forall b a t s . (Integral s, Num a, Integral b, Num t) => Iso s t a b
integral = iso fromIntegral fromIntegral

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

