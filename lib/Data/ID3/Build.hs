module Data.ID3.Build where

import Control.Lens
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as E
import Data.Text.Encoding.Lens
import qualified Data.ByteString as BS
import Data.ByteString.Builder as B

writePadBS :: Int -> BS.ByteString -> Builder 
writePadBS size str
  | size < 0 = error "Pad size is negative"
  | size < BS.length str = byteString $ BS.take size str
  | otherwise =
      let extralen = size - BS.length str
      in byteString . BS.append str . BS.concat . L.replicate extralen $ BS.singleton 0

writeText :: T.Text -> Builder
writeText = byteString . encode Utf8

writePadText :: Int -> T.Text -> B.Builder
writePadText size text =
  let encode = E.encodeUtf8 
  in writePadBS size . encode $ text

