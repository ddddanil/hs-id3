module Data.ID3.V2.Encoding where

import qualified Data.Text.Encoding as E
import Control.Lens.Iso
import Data.ID3.ReadWrite

data FrameEnc 
  = EncLatin1
  | EncUCS2
  | EncUTF16BE
  | EncUTF8
  deriving (Eq, Show, Enum)

getEncoder :: FrameEnc -> (Text -> ByteString)
getEncoder EncLatin1 = E.encodeUtf8
getEncoder EncUCS2 = E.encodeUtf16LE -- ???
getEncoder EncUTF16BE = E.encodeUtf16BE
getEncoder EncUTF8 = E.encodeUtf8

getDecoder :: FrameEnc -> (ByteString -> Text)
getDecoder EncLatin1 = E.decodeUtf8
getDecoder EncUCS2 = E.decodeUtf16LE -- ???
getDecoder EncUTF16BE = E.decodeUtf16BE
getDecoder EncUTF8 = E.decodeUtf8

encoded :: FrameEnc -> Iso' Text ByteString
encoded e = iso (getEncoder e) (getDecoder e)

decoded :: FrameEnc -> Iso' ByteString Text
decoded e = from $ encoded e
