module Data.ID3.V1.Parse where

import qualified Data.Text as T
import Control.Lens.Getter
import Control.Lens.Setter
import Data.Generics.Product.Fields
import Control.Monad.Combinators
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer
import Data.ID3.V1.Tag
import Data.ID3.Parse
import Data.ID3.Genre
import Data.ID3.ReadWrite

parseID3v1xTag :: Parser ID3v1xTag
parseID3v1xTag = do
  parseString @Text "TAG"
  buf_pos .= 128 - 3
  title <- parseTextField 30 <?> "title"
  artist <- parseTextField 30 <?> "artist"
  album <- parseTextField 30 <?> "album"
  year <- toText <$> replicateM 4 (w8toC <$> digitChar)
  buf_pos -= 4
  (comment, track) <- do
    comment <- parseTextField 28 <?> "comment"
    choice [ do { _zbyte <- try $ char 0 >> buf_pos -= 1;
                  track <- pByte;
                  return (comment, Just track)
          }, do { cont <- parseTextField 2 <?> "comment";
                  return (comment `T.append` cont, Nothing)
          }]
  genre <- parse @Genre
  guard =<< uses buf_pos (== 0) <?> "wrong tag length"
  return $ ID3v1xTag title artist album year comment track genre

_parseID3v1ETag :: Parser (Text -> Text -> Maybe Word8 -> ID3v1ETag)
_parseID3v1ETag = do
  parseString @Text "TAG+"
  buf_pos .= 227 - 4
  title <- parseTextField 60 <?> "title"
  artist <- parseTextField 60 <?> "artist"
  album <- parseTextField 60 <?> "album"
  speed <- pByte
  genre <- parseTextField 30 <?> "genre"
  [start_time, end_time] <- replicateM 2 $ do
    minutes <- decimal <?> "minutes"
    guard $ minutes < 1000
    _sep <- char . toEnum . fromEnum $ ':'
    seconds <- decimal <?> "seconds"
    guard $ seconds < 60
    buf_pos -= 6
    return $ (minutes, seconds)
  guard =<< uses buf_pos (== 0) <?> "wrong tag length"
  return $ ID3v1ETag title artist album speed genre start_time end_time

parseID3v1ETag :: Parser ID3v1ETag
parseID3v1ETag = do
  tag <- _parseID3v1ETag
  v11 <- parseID3v1xTag
  return $ tag (v11^.field @"year") (v11^.field @"comment") (v11^.field @"track")

_parseID3v12Tag :: Parser (Text, Text, Text, Text, Text)
_parseID3v12Tag = do
  parseString @Text "EXT"
  buf_pos .= 128 - 3
  title <- parseTextField 30 <?> "title"
  artist <- parseTextField 30 <?> "artist"
  album <- parseTextField 30 <?> "album"
  comment <- parseTextField 15 <?> "comment"
  subgenre <- parseTextField 20 <?> "subgenre"
  guard =<< uses buf_pos (== 0) <?> "wrong tag length"
  return $ (title, artist, album, comment, subgenre)

parseID3v12Tag :: Parser ID3v12Tag
parseID3v12Tag = do
  (_title, _artist, _album, _comment, subgenre) <- _parseID3v12Tag
  v11 <- parseID3v1xTag
  return $ ID3v12Tag
    ((v11^.field @"title") `T.append` _title)
    ((v11^.field @"artist") `T.append` _artist)
    ((v11^.field @"album") `T.append` _album)
    ((v11^.field @"comment") `T.append` _comment)
    subgenre (v11^.field @"year") (v11^.field @"track") (v11^.field @"genre")

