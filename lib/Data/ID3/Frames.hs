module Data.ID3.Frames where

import ClassyPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import Control.Lens.TH


-- Encoding

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

data Frame
  -- File ID frame
  = FileIDFrame
    { _fileIdFrameOwnerId :: T.Text
    , _fileIdFrameFileId :: BS.ByteString 
    }
  -- Text frame
  | GeneralTextFrame
    { _generalTextFrameEncoding :: FrameEnc
    , _generalTextFrameText :: T.Text 
    }
  | UserTextFrame
    { _userTextFrameEncoding :: FrameEnc
    , _userTextFrameDescription :: T.Text
    , _userTextFrameText :: T.Text
    }
  -- URL frame
  | GeneralUrlFrame
    { _generalUrlFrameUrl :: T.Text
    }
  | UserUrlFrame
    { _userUrlFrameEncoding :: FrameEnc
    , _userUrlFrameDescription :: T.Text
    , _userUrlFrameFrameUrl :: T.Text
    }
  -- Binary frame
  | BinaryFrame
    { _binaryFrameData :: BS.ByteString 
    }
-- TODO add frames
  deriving (Eq, Show)
makeFields ''Frame
makePrisms ''Frame