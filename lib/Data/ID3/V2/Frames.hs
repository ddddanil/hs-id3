module Data.ID3.Frames where

import Data.Generic
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Control.Lens.TH

data FileIdFrame = FileIdFrame
  { _fileIdFrameOwnerId :: T.Text
  , _fileIdFrameFileId :: BS.ByteString 
  }
  deriving (Eq, Show, Generic)

data GeneralTextFrame = GeneralTextFrame
  { _generalTextFrameEncoding :: FrameEnc
  , _generalTextFrameText :: T.Text 
  }
  deriving (Eq, Show, Generic)

data UserTextFrame = UserTextFrame
  { _userTextFrameEncoding :: FrameEnc
  , _userTextFrameDescription :: T.Text
  , _userTextFrameText :: T.Text
  }
  deriving (Eq, Show, Generic)

data GeneralUrlFrame = GeneralUrlFrame
  { _generalUrlFrameUrl :: T.Text
  }
  deriving (Eq, Show, Generic)

data UserUrlFrame = UserUrlFrame
  { _userUrlFrameEncoding :: FrameEnc
  , _userUrlFrameDescription :: T.Text
  , _userUrlFrameFrameUrl :: T.Text
  }
  deriving (Eq, Show, Generic)

data BinaryFrame = BinaryFrame
  { _binaryFrameData :: BS.ByteString 
  }
  deriving (Eq, Show, Generic)

makeFields ''FileIdFrame
makeFields ''GeneralTextFrame
makeFields ''UserTextFrame
makeFields ''GeneralUrlFrame
makeFields ''UserUrlFrame
makeFields ''BinaryFrame
-- TODO: More Frames
