module Data.ID3.V1.Tag.V12 (
  ID3v12Tag(ID3v12Tag)
) where

import Control.Lens
import Data.Generics.Product
import Text.Megaparsec hiding (parse)
import qualified Data.Text as T
import qualified Data.ByteString.Builder as B
import Data.ID3.Parse
import Data.ID3.Build
import Data.ID3.ReadWrite
import Data.ID3.V1.Tag.V11

data ID3v12Tag = ID3v12Tag
  { v11tag :: ID3v11Tag
  , subgenre :: !Text
  }
  deriving (Eq, Show, Generic)

parseID3v12Tag :: Parser ID3v12Tag
parseID3v12Tag = do
  parseString @Text "EXT"
  title <- parseTextField 30 <?> "title"
  artist <- parseTextField 30 <?> "artist"
  album <- parseTextField 30 <?> "album"
  comment <- parseTextField 15 <?> "comment"
  subgenre <- parseTextField 20 <?> "subgenre"
  v11tag <- parse @ID3v11Tag
      <&> the @"v10tag" . the @"title" %~ (`T.append` title)
      <&> the @"v10tag" . the @"artist" %~ (`T.append` artist)
      <&> the @"v10tag" . the @"album" %~ (`T.append` album)
      <&> the @"v10tag" . the @"comment" %~ (`T.append` comment)
  return $ ID3v12Tag v11tag subgenre

writeID3v12Tag :: ID3v12Tag -> B.Builder 
writeID3v12Tag tag =
  putSText "EXT"
  <> putPadText 30 (tag ^. dropping 30 (the @"v11tag" . the @"v10tag" . the @"title"))
  <> putPadText 30 (tag ^. dropping 30 (the @"v11tag" . the @"v10tag" . the @"artist"))
  <> putPadText 30 (tag ^. dropping 30 (the @"v11tag" . the @"v10tag" . the @"album"))
  <> putPadText 15 (tag ^. dropping 28 (the @"v11tag" . the @"v10tag" . the @"comment"))
  <> putPadText 20 (tag ^. the @"subgenre")
  <> write (tag ^. the @"v11tag")

instance ReadWrite ID3v12Tag where
  parse = parseID3v12Tag
  write = writeID3v12Tag
