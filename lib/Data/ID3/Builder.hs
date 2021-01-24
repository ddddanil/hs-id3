module Data.ID3.Builder (
  putID3v1Tag
) where

import Protolude as P hiding (try, putText)
import Protolude.Error (error)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.TH


import Data.ID3.Tag
import Data.ID3.Genre hiding (genre)

putPadBS :: Int -> BS.ByteString -> B.Builder 
putPadBS size str
  | size < 0 = error "Pad size is negative"
  | size < BS.length str = B.byteString $ BS.take size str
  | otherwise =
      let extralen = size - BS.length str
      in B.byteString . BS.append str . BS.concat . L.replicate extralen $ BS.singleton 0

putText :: T.Text -> B.Builder
putText text =
  let encode = E.encodeUtf8 
  in B.byteString . encode $ text

putPadText :: Int -> T.Text -> B.Builder
putPadText size text =
  let encode = E.encodeUtf8 
  in putPadBS size . encode $ text

putGenre :: Genre -> B.Builder 
putGenre (Genre g) = B.word8 g

putID3v1Tag :: ID3v1Tag -> B.Builder
putID3v1Tag tag =
  putText "TAG"
  <> putPadText 30 (tag^.title)
  <> putPadText 30 (tag^.artist)
  <> putPadText 30 (tag^.album)
  <> putText (tag^.year)
  <> case (tag^.track) of
       Nothing -> putPadText 30 (tag^.comment)
       Just tr -> putPadText 28 (tag^.comment) <> B.word8 0 <> B.word8 tr
  <> putGenre (tag^.genre)

