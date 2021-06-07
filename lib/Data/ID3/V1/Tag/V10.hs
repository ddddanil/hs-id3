module Data.ID3.V1.Tag.V10 where

import Control.Lens
import Control.Lens.TH
import Data.Generics.Product.Fields
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Byte
import qualified Data.ByteString.Builder as B
import Data.ID3.Parse
import Data.ID3.Build
import Data.ID3.ReadWrite
import Data.ID3.V1.Genre


data ID3v10Tag = ID3v10Tag
  { title :: !Text
  , artist :: !Text
  , album :: !Text
  , year :: !Text
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
  year <- toText <$> replicateM 4 (w8toC <$> digitChar)
  comment <- parseTextField 30 <?> "comment"
  genre <- parse @Genre
  return $ ID3v10Tag title artist album year comment genre

writeID3v10Tag :: ID3v10Tag -> B.Builder
writeID3v10Tag tag =
  putSText "TAG"
  <> putPadText 30 (tag ^. field @"title")
  <> putPadText 30 (tag ^. field @"artist")
  <> putPadText 30 (tag ^. field @"album")
  <> putSText (tag ^. field @"year")
  <> putPadText 30 (tag ^. field @"comment")
  <> write (tag ^. field @"genre")

instance ReadWrite ID3v10Tag where
  parse = parseID3v10Tag
  write = writeID3v10Tag
