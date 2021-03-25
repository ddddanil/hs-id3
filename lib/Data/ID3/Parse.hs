module Data.ID3.Parse where

import qualified Data.Text.Lazy as T
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.TH
import Control.Monad.Combinators
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer
import Data.ID3.Tag
import Data.ID3.Genre hiding (genre)

data ParserOpts = ParserOpts
  { _encoder :: Text -> ByteString
  , _decoder :: ByteString -> LText
  , _buf_pos :: Int
  }
makeLenses ''ParserOpts

type Parser = ParsecT Void ByteString (State ParserOpts) -- Add state

runTagParser :: Parser a -> Text -> ByteString -> Either (ParseErrorBundle ByteString Void) a
runTagParser p n s = evalState (runParserT p (toString n) s) (ParserOpts encodeUtf8 decodeUtf8 0)

runTagParser_ :: Parser a -> Text -> ByteString -> Maybe a
runTagParser_ p n = rightToMaybe . runTagParser p n

w8toC :: Word8 -> Char 
w8toC = chr . fromEnum

pByte :: Parser Word8
pByte = do
  byte <- anySingle 
  buf_pos -= 1
  return byte

traceBufPos :: Parser ()
traceBufPos = traceShowM =<< use buf_pos

parseTextField :: Int -> Parser LText
parseTextField size = do
  decode <- use decoder
  text <- decode <$> takeP Nothing size
  buf_pos -= size
  return $ T.takeWhile (\c -> c /= chr 0) text

parseGenre :: Parser Genre
parseGenre = do
  g <- pByte
  guard (g <= 191) <?> "valid genre byte"
  return $ Genre g

parseID3v1xTag :: Parser ID3v1xTag
parseID3v1xTag = do
  encode <- use encoder
  _ <- string $ encode "TAG"
  buf_pos .= 128 - 3
  title <- parseTextField 30 <?> "title"
  artist <- parseTextField 30 <?> "artist"
  album <- parseTextField 30 <?> "album"
  year <- toLText <$> replicateM 4 (w8toC <$> digitChar)
  buf_pos -= 4
  (comment, track) <- do
    comment <- parseTextField 28 <?> "comment"
    choice [ do { _zbyte <- try $ char 0 >> buf_pos -= 1;
                  track <- pByte;
                  return (comment, Just track)
          }, do { cont <- parseTextField 2 <?> "comment";
                  return (comment `T.append` cont, Nothing)
          }]
  genre <- parseGenre 
  guard =<< uses buf_pos (== 0) <?> "wrong tag length"
  return $ ID3v1xTag title artist album year comment track genre

_parseID3v1ETag :: Parser (LText -> LText -> Maybe Word8 -> ID3v1ETag)
_parseID3v1ETag = do
  encode <- use encoder
  _ <- string $ encode "TAG+"
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
  return $ tag (v11^.year) (v11^.comment) (v11^.track)

_parseID3v12Tag :: Parser (LText, LText, LText, LText, LText)
_parseID3v12Tag = do
  encode <- use encoder
  _ <- string $ encode "EXT"
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
    ((v11^.title) `T.append` _title)
    ((v11^.artist) `T.append` _artist)
    ((v11^.album) `T.append` _album)
    ((v11^.comment) `T.append` _comment)
    subgenre (v11^.year) (v11^.track) (v11^.genre)
