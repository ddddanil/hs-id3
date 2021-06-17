module Data.ID3.V1.Tag.V10 (
  ID3v10Tag(ID3v10Tag)
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
import Data.ID3.V1.Genre


data ID3v10Tag = ID3v10Tag
  { title :: !Text
  , artist :: !Text
  , album :: !Text
  , year :: !Word16
  , comment :: !Text
  , genre :: !Genre
  }
  deriving (Eq, Show, Generic)


parseID3v10Tag :: Parser ID3v10Tag
parseID3v10Tag = do
  parseString "TAG"
  title <- parseTextField 30 <?> "title"
  artist <- parseTextField 30 <?> "artist"
  album <- parseTextField 30 <?> "album"
  year_ <- takeP (Just "decimal") 4
  year <- withInput year_ decimal
  comment <- parseTextField 30 <?> "comment"
  genre <- parse @Genre
  return $ ID3v10Tag title artist album year comment genre

writeID3v10Tag :: ID3v10Tag -> Builder
writeID3v10Tag tag =
  writeText "TAG"
  <> writePadText 30 (tag ^. field @"title")
  <> writePadText 30 (tag ^. field @"artist")
  <> writePadText 30 (tag ^. field @"album")
  <> word16Dec (tag ^. field @"year")
  <> writePadText 30 (tag ^. field @"comment")
  <> write (tag ^. field @"genre")

instance ReadWrite ID3v10Tag where
  parse = parseID3v10Tag
  write = writeID3v10Tag

prettyID3v10Tag :: ID3v10Tag -> Doc ann
prettyID3v10Tag tag =
  pretty @Text "ID3 v1 Tag" <> line
  <> (indent 4 . vsep $
    [ (fill 8 . pretty @Text $ "Version:") <+> pretty @Text "1.0"
    , (fill 8 . pretty @Text $ "Title:") <+> pretty (tag ^. the @"title")
    , (fill 8 . pretty @Text $ "Artist:") <+> pretty (tag ^. the @"artist")
    , (fill 8 . pretty @Text $ "Album:") <+> pretty (tag ^. the @"album")
    , (fill 8 . pretty @Text $ "Year:") <+> pretty (tag ^. the @"year")
    , (fill 8 . pretty @Text $ "Comment:") <+> pretty (tag ^. the @"comment")
    , (fill 8 . pretty @Text $ "Genre:") <+> pretty (tag ^. the @"genre")
    ])

instance Pretty ID3v10Tag where
  pretty = prettyID3v10Tag
