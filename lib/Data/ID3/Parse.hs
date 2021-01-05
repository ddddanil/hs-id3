module Data.ID3.Parse where

import Protolude as P hiding (try)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import Control.Lens.Getter
import Control.Lens.TH
import Control.Monad.Combinators
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer
import Data.ID3.Tag
import Data.ID3.Genre hiding (genre)

data ParserOpts = ParserOpts
  { _encoder :: T.Text -> BS.ByteString 
  , _decoder :: BS.ByteString -> T.Text 
  }
makeLenses ''ParserOpts

type Parser = ParsecT Void BS.ByteString (State ParserOpts) -- Add state

runTagParser :: Parser a -> BS.ByteString -> Either (ParseErrorBundle BS.ByteString Void) a
runTagParser p s = evalState (runParserT p "<file>" s) (ParserOpts E.encodeUtf8 E.decodeUtf8)

w8toC :: Word8 -> Char 
w8toC = chr . fromEnum

parseTextField :: Int -> Parser T.Text
parseTextField size = do
  decode <- use decoder
  text <- decode <$> takeP Nothing size
  return $ T.takeWhile (\c -> c /= chr 0) text

parseGenre :: Parser Genre
parseGenre = do
  g <- anySingle
  guard (g <= 191) <?> "valid genre byte"
  return $ Genre g

parseID3v1Tag :: Parser ID3v1Tag 
parseID3v1Tag = do
  encode <- use encoder
  _ <- string $ encode "TAG"
  title <- parseTextField 30 <?> "title"
  artist <- parseTextField 30 <?> "artist"
  album <- parseTextField 30 <?> "album"
  year <- T.pack <$> replicateM 4 (w8toC <$> digitChar)
  (comment, track) <- do
    comment <- parseTextField 28 <?> "comment"
    choice [ do { _zbyte <- try $ char 0;
                  track <- anySingle;
                  return (comment, Just track)
          }, do { cont <- parseTextField 2 <?> "comment";
                  return (comment `T.append` cont, Nothing)
          }]
  genre <- parseGenre 
  return $ ID3v1Tag title artist album year comment track genre

_parseID3v1ETag :: Parser (T.Text -> T.Text -> Maybe Word8 -> ID3v1ETag)
_parseID3v1ETag = do
  encode <- use encoder
  _ <- string $ encode "TAG+"
  title <- parseTextField 60 <?> "title"
  artist <- parseTextField 60 <?> "artist"
  album <- parseTextField 60 <?> "album"
  speed <- anySingle
  genre <- parseTextField 30 <?> "genre"
  [start_time, end_time] <- replicateM 2 $ do
    minutes <- decimal <?> "minutes"
    guard $ minutes < 1000
    _sep <- char . toEnum . fromEnum $ ':'
    seconds <- decimal <?> "seconds"
    guard $ seconds < 60
    return $ (minutes, seconds)
  return $ ID3v1ETag title artist album speed genre start_time end_time

parseID3v1ETag :: Parser ID3v1ETag
parseID3v1ETag = do
  tag <- _parseID3v1ETag
  v11 <- parseID3v1Tag
  return $ tag (v11^.year) (v11^.comment) (v11^.track)

_parseID3v12Tag :: Parser (T.Text, T.Text, T.Text, T.Text, T.Text)
_parseID3v12Tag = do
  encode <- use encoder
  _ <- string $ encode "EXT"
  title <- parseTextField 30 <?> "title"
  artist <- parseTextField 30 <?> "artist"
  album <- parseTextField 30 <?> "album"
  comment <- parseTextField 15 <?> "comment"
  subgenre <- parseTextField 20 <?> "subgenre"
  return $ (title, artist, album, comment, subgenre)

parseID3v12Tag :: Parser ID3v12Tag
parseID3v12Tag = do
  (_title, _artist, _album, _comment, subgenre) <- _parseID3v12Tag
  v11 <- parseID3v1Tag
  return $ ID3v12Tag
    ((v11^.title) `T.append` _title)
    ((v11^.artist) `T.append` _artist)
    ((v11^.album) `T.append` _album)
    ((v11^.comment) `T.append` _comment)
    subgenre (v11^.year) (v11^.track) (v11^.genre)
