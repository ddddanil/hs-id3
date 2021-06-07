module Data.Bits.Lens.Extra where

import Data.Bits
import Control.Lens

shiftedL :: (Bits a, Bits b) => Int -> Iso a b a b
shiftedL n = iso (`shiftL` n) (`shiftR` n)

shiftedR :: (Bits a, Bits b) => Int -> Iso a b a b
shiftedR n = iso (`shiftR` n) (`shiftL` n)
