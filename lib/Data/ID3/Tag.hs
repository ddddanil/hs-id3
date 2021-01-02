module Data.ID3.Tag (
  ID3Ver(..)
, ID3v1Tag
, mkID3v1Tag
) where

import Protolude
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Control.Lens.TH

-- | ID3 Supported versions
data ID3Ver
  = ID3v1  -- ^ v1 and v1.1
  | ID3v1E -- ^ unofficial Enhanced TAG
  | ID3v22 -- ^ v2.2
  | ID3v23 -- ^ v2.3
  | ID3v24 -- ^ v2.4

textByteLength :: (T.Text -> BS.ByteString) -> T.Text -> Int
textByteLength encoding = BS.length . encoding

data ID3v1Tag = ID3v1Tag
  { title :: T.Text
  , artist :: T.Text 
  , album :: T.Text
  , year :: T.Text
  , comment :: T.Text
  , track :: Maybe Word8
  , genre :: Word8 -- Do custom Type
  }
  deriving (Eq, Show)

mkID3v1Tag ::
  (T.Text -> BS.ByteString) -- ^ encoding func
  -> T.Text       -- ^ title
  -> T.Text       -- ^ artist
  -> T.Text       -- ^ album
  -> T.Text       -- ^ year
  -> T.Text       -- ^ comment
  -> Maybe Word8  -- ^ track
  -> Word8        -- ^ genre
  -> Maybe ID3v1Tag
mkID3v1Tag encoding title artist album year comment track genre
  | textByteLength encoding title > 30 = Nothing
  | textByteLength encoding artist > 30 = Nothing
  | textByteLength encoding album > 30 = Nothing
  | T.length year /= 4 || not (T.all isDigit year) = Nothing
  | textByteLength encoding comment > 30 = Nothing
  | isJust track && textByteLength encoding comment > 28 = Nothing
  | otherwise = Just $ ID3v1Tag { .. }
