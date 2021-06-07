module GHC.Real.Lens where

import Control.Lens

integral :: forall b a t s . (Integral s, Num a, Integral b, Num t) => Iso s t a b
integral = iso fromIntegral fromIntegral
