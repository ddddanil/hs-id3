module Data.ID3.V1.Tag.V12 (
  ID3v12Tag(ID3v12Tag)
) where

import Control.Lens
import Data.Generics.Product
import Text.Megaparsec hiding (parse)
import qualified Data.Text as T
import Data.ByteString.Builder
import Prettyprinter hiding ((<>))
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
  parseString "EXT"
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

writeID3v12Tag :: ID3v12Tag -> Builder 
writeID3v12Tag tag =
  writeText "EXT"
  <> writePadText 30 (tag ^. dropping 30 (the @"v11tag" . the @"v10tag" . the @"title"))
  <> writePadText 30 (tag ^. dropping 30 (the @"v11tag" . the @"v10tag" . the @"artist"))
  <> writePadText 30 (tag ^. dropping 30 (the @"v11tag" . the @"v10tag" . the @"album"))
  <> writePadText 15 (tag ^. dropping 28 (the @"v11tag" . the @"v10tag" . the @"comment"))
  <> writePadText 20 (tag ^. the @"subgenre")
  <> write (tag ^. the @"v11tag")

instance ReadWrite ID3v12Tag where
  parse = parseID3v12Tag
  write = writeID3v12Tag


prettyID3v12Tag :: ID3v12Tag -> Doc ann
prettyID3v12Tag tag =
  pretty @Text "ID3 v1 Tag" <> line
  <> (indent 4 . vsep $
    [ (fill 8 . pretty @Text $ "Version:") <+> pretty @Text "1.2"
    , (fill 8 . pretty @Text $ "Title:") <+> pretty (tag ^. the @"v11tag" . the @"v10tag" . the @"title")
    , (fill 8 . pretty @Text $ "Artist:") <+> pretty (tag ^. the @"v11tag" . the @"v10tag" . the @"artist")
    , (fill 8 . pretty @Text $ "Album:") <+> pretty (tag ^. the @"v11tag" . the @"v10tag" . the @"album")
    , (fill 8 . pretty @Text $ "Year:") <+> pretty (tag ^. the @"v11tag" . the @"v10tag" . the @"year")
    , (fill 8 . pretty @Text $ "Comment:") <+> pretty (tag ^. the @"v11tag" . the @"v10tag" . the @"comment")
    , (fill 8 . pretty @Text $ "Track:") <+> pretty (tag ^. the @"v11tag" . the @"track")
    , (fill 8 . pretty @Text $ "Genre:") <+> pretty (tag ^. the @"v11tag" . the @"v10tag" . the @"genre")
    , (fill 8 . pretty @Text $ "Subgenre:") <+> pretty (tag ^. the @"subgenre")
    ])

instance Pretty ID3v12Tag where
  pretty = prettyID3v12Tag
