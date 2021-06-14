module Data.ID3.V1.Tag where

import Control.Lens
import Control.Applicative.Combinators hiding (optional)
import Data.Generics.Product
import Text.Megaparsec as MP hiding (parse)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.ByteString.Builder
import Data.ID3.Parse
import Data.ID3.Build
import Data.ID3.ReadWrite
import Data.ID3.Version
import Data.ID3.V1.Genre
import Data.ID3.V1.Tag.V10
import Data.ID3.V1.Tag.V11
import Data.ID3.V1.Tag.V12
import Data.ID3.V1.Tag.V1E


data ID3v1Tag
  = ID3v10 !ID3v10Tag
  | ID3v11 !ID3v11Tag
  | ID3v12 !ID3v12Tag
  | ID3v1E !ID3v1ETag
  deriving (Eq, Show, Generic)
makePrisms ''ID3v1Tag

-- TODO change to unlawful lens with conversions
v1version :: Getter ID3v1Tag ID3v1Ver
v1version = to getVer
  where
    getVer :: ID3v1Tag -> ID3v1Ver
    getVer (ID3v10 _) = ID3v10Ver
    getVer (ID3v11 _) = ID3v11Ver
    getVer (ID3v12 _) = ID3v12Ver
    getVer (ID3v1E _) = ID3v1EVer

version :: Getter ID3v1Tag ID3Version
version = v1version . re _ID3v1Version

v10tag :: Lens' ID3v1Tag ID3v10Tag
v10tag = lens getTag setTag
  where
    getTag :: ID3v1Tag -> ID3v10Tag
    getTag (ID3v10 tag) = tag
    getTag (ID3v11 tag) = tag ^. the @"v10tag"
    getTag (ID3v12 tag) = tag ^. the @"v11tag" . the @"v10tag"
    getTag (ID3v1E tag) = tag ^. the @"v11tag" . the @"v10tag"
    setTag :: ID3v1Tag -> ID3v10Tag -> ID3v1Tag
    setTag tag v10 = tag
        & _ID3v10 .~ v10
        & _ID3v11 . the @"v10tag" .~ v10
        & _ID3v12 . the @"v11tag" . the @"v10tag" .~ v10
        & _ID3v1E . the @"v11tag" . the @"v10tag" .~ v10

v11tag :: Traversal' ID3v1Tag ID3v11Tag
v11tag = _ID3v11 `failing` _ID3v12 . the @"v11tag" `failing` _ID3v1E . the @"v11tag"

title :: Lens' ID3v1Tag Text
title = v10tag . the @"title"

artist :: Lens' ID3v1Tag Text
artist = v10tag . the @"artist"

album :: Lens' ID3v1Tag Text
album = v10tag . the @"album"

comment :: Lens' ID3v1Tag Text
comment = v10tag . the @"comment"

year :: Lens' ID3v1Tag Word16
year = v10tag . the @"year"

genre :: Lens' ID3v1Tag Genre
genre = v10tag . the @"genre"

track :: Traversal' ID3v1Tag Word8
track = v11tag . the @"track"

subgenre :: Traversal' ID3v1Tag Text
subgenre = _ID3v1E . the @"genre" `failing` _ID3v12 . the @"subgenre"

parseID3v1Tag :: Parser ID3v1Tag
parseID3v1Tag = choice
  [ parse @ID3v1ETag <&> (_ID3v1E#)
  , parse @ID3v12Tag <&> (_ID3v12#)
  , parse @ID3v11Tag <&> (_ID3v11#)
  , parse @ID3v10Tag <&> (_ID3v10#)
  ]

-- writeID3v1Tag :: ID3v1Tag -> Builder
-- writeID3v1Tag tag = tag
--   & _ID3v10 %~ write
--   & _ID3v11 %~ write
--   & _ID3v12 %~ write
--   & _ID3v1E %~ write

splitAtEnd :: Int -> ByteString -> (ByteString, ByteString)
splitAtEnd n s = BS.splitAt (BS.length s - n) s

parseID3v1 :: Parser (ParseResult ID3v1Tag)
parseID3v1 = do
  let v10len = 128
      v1Elen = v10len + 227
      v12len = v10len + 128
  input <- getInput
  x <- optional . choice $
    [ splitAtEnd v1Elen input
        <&> (\s -> withInput s (parse @ID3v1ETag) <&> (_ID3v1E#))
        & sequenceA
    , splitAtEnd v12len input
        <&> (\s -> withInput s (parse @ID3v12Tag) <&> (_ID3v12#))
        & sequenceA
    , splitAtEnd v10len input
        <&> (\s -> withInput s (parse @ID3v11Tag) <&> (_ID3v11#))
        & sequenceA
    , splitAtEnd v10len input
        <&> (\s -> withInput s (parse @ID3v10Tag) <&> (_ID3v10#))
        & sequenceA
    ]
  return . (_ParseResult #) . fromMaybe (input, Nothing) $ (Just <<$>> x)
