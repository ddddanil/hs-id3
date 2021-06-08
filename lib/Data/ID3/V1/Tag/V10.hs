module Data.ID3.V1.Tag.V10 (
  ID3v10Tag(ID3v10Tag)
) where

import Control.Lens
import Data.Generics.Product.Fields
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Byte.Lexer
import qualified Data.ByteString.Builder as B
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
  parseString @Text "TAG"
  title <- parseTextField 30 <?> "title"
  artist <- parseTextField 30 <?> "artist"
  album <- parseTextField 30 <?> "album"
  year_ <- takeP (Just "decimal") 4
  year <- withInput year_ decimal
  comment <- parseTextField 30 <?> "comment"
  genre <- parse @Genre
  return $ ID3v10Tag title artist album year comment genre

writeID3v10Tag :: ID3v10Tag -> B.Builder
writeID3v10Tag tag =
  putSText "TAG"
  <> putPadText 30 (tag ^. field @"title")
  <> putPadText 30 (tag ^. field @"artist")
  <> putPadText 30 (tag ^. field @"album")
  <> B.word16Dec (tag ^. field @"year")
  <> putPadText 30 (tag ^. field @"comment")
  <> write (tag ^. field @"genre")

instance ReadWrite ID3v10Tag where
  parse = parseID3v10Tag
  write = writeID3v10Tag
