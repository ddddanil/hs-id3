module Data.List.Lens.Extra where

import Control.Lens

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

asChunks :: Int -> Iso [a] [b] [[a]] [[b]]
asChunks n = iso (chunks n) concat
