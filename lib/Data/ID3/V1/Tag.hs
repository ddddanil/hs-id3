module Data.ID3.V1.Tag where

import Control.Lens
import Data.Generics.Product
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

-- v11tag :: Lens' ID3v1Tag (Maybe ID3v11Tag)
-- v11tag = lens setTag getTag
--   where
--     getTag :: ID3v1Tag -> (Maybe ID3v11Tag)
--     getTag (ID3v10 _) = Nothing
--     getTag (ID3v11 tag) = Just tag
--     getTag (ID3v12 tag) = tag ^. the @"v11tag" . re _Just
--     getTag (ID3v1E tag) = tag ^. the @"v11tag" . re _Just
--     setTag :: ID3v1Tag -> ID3v11Tag -> ID3v1Tag
--     setTag (ID3v10)
--     setTag tag v11 = tag
--         & _ID3v11 .~ v11
--         & _ID3v12 . the @"v11tag" .~ v11
--         & _ID3v1E . the @"v11tag" .~ v11

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


