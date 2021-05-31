module Data.ID3.Build where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.TH

putPadBS :: Int -> BS.ByteString -> B.Builder 
putPadBS size str
  | size < 0 = error "Pad size is negative"
  | size < BS.length str = B.byteString $ BS.take size str
  | otherwise =
      let extralen = size - BS.length str
      in B.byteString . BS.append str . BS.concat . L.replicate extralen $ BS.singleton 0

putSText :: T.Text -> B.Builder
putSText text =
  let encode = E.encodeUtf8 
  in B.byteString . encode $ text

putLText :: LT.Text -> B.Builder
putLText = putSText . fromLazy

putPadText :: Int -> T.Text -> B.Builder
putPadText size text =
  let encode = E.encodeUtf8 
  in putPadBS size . encode $ text

putPadLText :: Int -> LT.Text -> B.Builder
putPadLText size text = putPadText size $ fromLazy text 


