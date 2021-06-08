module Data.ID3.V1.Tag.V11 (
  ID3v11Tag(ID3v11Tag)
) where

import Control.Lens
import Data.Generics.Product
import Data.Text.Encoding.Lens
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Byte.Lexer
import Data.ByteString.Builder
import Data.ID3.Parse
import Data.ID3.Build
import Data.ID3.ReadWrite
import Data.ID3.V1.Genre
import Data.ID3.V1.Tag.V10

data ID3v11Tag = ID3v11Tag
  { v10tag :: ID3v10Tag
  , track :: Word8
  }
  deriving (Eq, Show, Generic)

-- noTrack :: Maybe a -> Bool
-- noTrack Nothing = True
-- noTrack (Just 0) = True
-- noTrack _ = False

-- pattern NoTrack :: Maybe a
-- pattern NoTrack <- (noTrack -> True)

-- _ID3v11Tag :: Prism' ID3v10Tag ID3v11Tag
-- _ID3v11Tag = prism' unwrap wrap
--   where
--     unwrap :: ID3v11Tag -> ID3v10Tag
--     unwrap t = (t ^. the @"v10tag") & the @"comment" . encoded Utf8 . dropping 28 traversed .~ [0, t ^. the @"track"]
--     wrap :: ID3v10Tag -> Maybe ID3v11Tag
--     wrap t = case t ^. the @"comment" . encoded Utf8 . at 28 of
--       NoTrack -> Just . ID3v11Tag <$> t <*> 0
--       _ -> Nothing 

parseID3v11Tag :: Parser ID3v11Tag
parseID3v11Tag = do
  parseString "TAG"
  title <- parseTextField 30 <?> "title"
  artist <- parseTextField 30 <?> "artist"
  album <- parseTextField 30 <?> "album"
  year_ <- takeP (Just "decimal") 4
  year <- withInput year_ decimal
  comment <- parseTextField 28 <?> "comment"
  single 0
  track <- anySingle <?> "track"
  genre <- parse @Genre
  return $ ID3v11Tag (ID3v10Tag title artist album year comment genre) track

writeID3v11Tag :: ID3v11Tag -> Builder
writeID3v11Tag tag =
  writeText "TAG"
  <> writePadText 30 (tag ^. the @"v10tag" . the @"title")
  <> writePadText 30 (tag ^. the @"v10tag" . the @"artist")
  <> writePadText 30 (tag ^. the @"v10tag" . the @"album")
  <> word16Dec (tag ^. the @"v10tag" . the @"year")
  <> writePadText 28 (tag ^. the @"v10tag" . the @"comment")
  <> word8 0 <> word8 (tag ^. the @"track")
  <> write (tag ^. the @"v10tag" . the @"genre")

instance ReadWrite ID3v11Tag where
  parse = parseID3v11Tag
  write = writeID3v11Tag

