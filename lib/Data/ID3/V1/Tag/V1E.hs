module Data.ID3.V1.Tag.V1E where

import Control.Lens
import Data.Generics.Product
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Byte.Lexer
import qualified Data.ByteString.Builder as B
import Data.ID3.Parse
import Data.ID3.Build
import Data.ID3.ReadWrite
import Data.ID3.V1.Tag.V11

data TagTime = TagTime
  { minutes :: !Word 
  , seconds :: !Word
  }
  deriving (Eq, Show, Generic)

data ID3v1ETag = ID3v1ETag
  { v11tag :: ID3v11Tag
  , speed :: !Word8
  , genre :: !Text
  , startTime :: !TagTime
  , endTime :: !TagTime
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

writeTagTime :: TagTime -> B.Builder
writeTagTime t =
  B.wordDec (t ^. the @"minutes")
  <> B.char7 ':'
  <> B.wordDec (t ^. the @"seconds")

instance ReadWrite TagTime where
  parse = parseTagTime
  write = writeTagTime

parseID3v1ETag :: Parser ID3v1ETag
parseID3v1ETag = do
  parseString @Text "TAG+"
  title <- parseTextField 60 <?> "title"
  artist <- parseTextField 60 <?> "artist"
  album <- parseTextField 60 <?> "album"
  speed <- anySingle
  genre <- parseTextField 30 <?> "genre"
  [start_time, end_time] <- replicateM 2 parseTagTime
  v11 <- parseID3v11Tag
      <&> the @"v10tag" . the @"title" .~ title
      <&> the @"v10tag" . the @"artist" .~ artist
      <&> the @"v10tag" . the @"album" .~ album
  return $ ID3v1ETag v11 speed genre start_time end_time

writeID3v1ETag :: ID3v1ETag -> B.Builder 
writeID3v1ETag tag =
  putSText "TAG+"
  <> putPadText 60 (tag ^. dropping 30 (the @"v11tag" . the @"v10tag" . the @"title"))
  <> putPadText 60 (tag ^. dropping 30 (the @"v11tag" . the @"v10tag" . the @"artist"))
  <> putPadText 60 (tag ^. dropping 30 (the @"v11tag" . the @"v10tag" . the @"album"))
  <> B.word8 (tag ^. the @"speed")
  <> write (tag ^. the @"startTime")
  <> write (tag ^. the @"endTime")
  <> write (tag ^. the @"v11tag")

instance ReadWrite ID3v1ETag where
  parse = parseID3v1ETag
  write = writeID3v1ETag
