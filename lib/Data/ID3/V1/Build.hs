module Data.ID3.V1.Build where
     
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Control.Lens.Getter
import Control.Lens.Setter
import Data.Generics.Product.Fields
import Data.ID3.Build as BB
import Data.ID3.Genre
import Data.ID3.ReadWrite
import Data.ID3.V1.Tag

putID3v1Tag :: ID3v1xTag -> B.Builder
putID3v1Tag tag =
  putSText "TAG"
  <> putPadLText 30 (tag ^. field @"title")
  <> putPadLText 30 (tag ^. field @"artist")
  <> putPadLText 30 (tag ^. field @"album")
  <> BB.putLText (tag ^. field @"year")
  <> case (tag ^. field @"track") of
       Nothing -> putPadLText 30 (tag ^. field @"comment")
       Just tr -> putPadLText 28 (tag ^. field @"comment") <> B.word8 0 <> B.word8 tr
  <> write (tag ^. field @"genre")


