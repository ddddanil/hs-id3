module Data.ID3.V1.Tag.V1E (
  ID3v1ETag(ID3v1ETag)
, TagTime(TagTime)
) where

import Control.Lens
import Data.Generics.Product
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Byte.Lexer
import Data.ByteString.Builder
import Prettyprinter hiding ((<>))
import Data.ID3.Parse
import Data.ID3.Build
import Data.ID3.ReadWrite
import Data.ID3.V1.Tag.V11

data TagTime = TagTime
  { minutes :: !Word
  , seconds :: !Word
  }
  deriving (Eq, Show, Generic)

parseTagTime :: Parser TagTime
parseTagTime = do
  minutes_ <- takeP (Just "decimal") 3
  _sep <- single . toEnum . fromEnum $ ':'
  seconds_ <- takeP (Just "decimal") 3
  minutes <- withInput minutes_ decimal
  seconds <- withInput seconds_ decimal
  return $ TagTime minutes seconds

writeTagTime :: TagTime -> Builder
writeTagTime t =
  wordDec (t ^. the @"minutes")
  <> char7 ':'
  <> wordDec (t ^. the @"seconds")

instance ReadWrite TagTime where
  parse = parseTagTime
  write = writeTagTime

prettyTagTime :: TagTime -> Doc ann
prettyTagTime t = pretty (t ^. the @"minutes") <> colon <> pretty (t ^. the @"seconds")

instance Pretty TagTime where
  pretty = prettyTagTime

data ID3v1ETag = ID3v1ETag
  { v11tag :: ID3v11Tag
  , speed :: !Word8
  , genre :: !Text
  , startTime :: !TagTime
  , endTime :: !TagTime
  }
  deriving (Eq, Show, Generic)

parseID3v1ETag :: Parser ID3v1ETag
parseID3v1ETag = do
  parseString "TAG+"
  title <- parseTextField 60 <?> "title"
  artist <- parseTextField 60 <?> "artist"
  album <- parseTextField 60 <?> "album"
  speed <- anySingle
  genre <- parseTextField 30 <?> "genre"
  [start_time, end_time] <- replicateM 2 parseTagTime
  v11 <- parse @ID3v11Tag
      <&> the @"v10tag" . the @"title" .~ title
      <&> the @"v10tag" . the @"artist" .~ artist
      <&> the @"v10tag" . the @"album" .~ album
  return $ ID3v1ETag v11 speed genre start_time end_time

writeID3v1ETag :: ID3v1ETag -> Builder
writeID3v1ETag tag =
  writeText "TAG+"
  <> writePadText 60 (tag ^. dropping 30 (the @"v11tag" . the @"v10tag" . the @"title"))
  <> writePadText 60 (tag ^. dropping 30 (the @"v11tag" . the @"v10tag" . the @"artist"))
  <> writePadText 60 (tag ^. dropping 30 (the @"v11tag" . the @"v10tag" . the @"album"))
  <> word8 (tag ^. the @"speed")
  <> write (tag ^. the @"startTime")
  <> write (tag ^. the @"endTime")
  <> write (tag ^. the @"v11tag")

instance ReadWrite ID3v1ETag where
  parse = parseID3v1ETag
  write = writeID3v1ETag

prettyID3v1ETag :: ID3v1ETag -> Doc ann
prettyID3v1ETag tag =
  pretty @Text "ID3 v1 Tag" <> line
  <> (indent 4 . vsep $
    [ (fill 8 . pretty @Text $ "Version:") <+> pretty @Text "Enhanced TAG"
    , (fill 8 . pretty @Text $ "Title:") <+> pretty (tag ^. the @"v11tag" . the @"v10tag" . the @"title")
    , (fill 8 . pretty @Text $ "Artist:") <+> pretty (tag ^. the @"v11tag" . the @"v10tag" . the @"artist")
    , (fill 8 . pretty @Text $ "Album:") <+> pretty (tag ^. the @"v11tag" . the @"v10tag" . the @"album")
    , (fill 8 . pretty @Text $ "Year:") <+> pretty (tag ^. the @"v11tag" . the @"v10tag" . the @"year")
    , (fill 8 . pretty @Text $ "Comment:") <+> pretty (tag ^. the @"v11tag" . the @"v10tag" . the @"comment")
    , (fill 8 . pretty @Text $ "Track:") <+> pretty (tag ^. the @"v11tag" . the @"track")
    , (fill 8 . pretty @Text $ "Genre:") <+> pretty (tag ^. the @"v11tag" . the @"v10tag" . the @"genre")
    , (fill 8 . pretty @Text $ "Subgenre:") <+> pretty (tag ^. the @"genre")
    , (fill 8 . pretty @Text $ "Speed:") <+> prettySpeed (tag ^. the @"speed")
    , (fill 8 . pretty @Text $ "Start time:") <+> pretty (tag ^. the @"startTime")
    , (fill 8 . pretty @Text $ "End time:") <+> pretty (tag ^. the @"endTime")
    ])
  where
    prettySpeed :: Word8 -> Doc ann
    prettySpeed x@0 = (parens . pretty $ x) <+> pretty @Text "Unset"
    prettySpeed x@1 = (parens . pretty $ x) <+> pretty @Text "Slow"
    prettySpeed x@2 = (parens . pretty $ x) <+> pretty @Text "Medium"
    prettySpeed x@3 = (parens . pretty $ x) <+> pretty @Text "High"
    prettySpeed x@4 = (parens . pretty $ x) <+> pretty @Text "Hardcore"
    prettySpeed x = (parens . pretty $ x) <+> pretty @Text "Unknown"

instance Pretty ID3v1ETag where
  pretty = prettyID3v1ETag
