module Data.Bits.Lens.Extra where

import Data.Bits
import Control.Lens

shiftedL :: (Bits a, Bits b) => Int -> Iso a b a b
shiftedL n = iso (`shiftL` n) (`shiftR` n)

shiftedR :: (Bits a, Bits b) => Int -> Iso a b a b
shiftedR n = iso (`shiftR` n) (`shiftL` n)

reverseBits :: Word8 -> Word8
reverseBits x = fromIntegral x'
   where
      !x' = ((fromIntegral x * 0x0202020202 :: Word64) .&. 0x010884422010) `mod` 1023

reversedBits :: Iso' Word8 Word8
reversedBits = involuted reverseBits
