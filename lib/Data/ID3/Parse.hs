module Data.ID3.Parse where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.TH
import Data.Generics.Product.Fields
import Control.Monad.Combinators
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer

data ParserOpts = ParserOpts
  { _encoder :: Text -> ByteString
  , _decoder :: ByteString -> Text
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

{-
parseLTextField :: Int -> Parser LText
parseLTextField size = do
  decode <- use decoder
  text <- decode <$> takeP Nothing size
  buf_pos -= size
  return $ LT.takeWhile (\c -> c /= chr 0) text
-}

parseTextField :: Int -> Parser Text
parseTextField size = do
  decode <- use decoder
  text <- decode <$> takeP Nothing size
  buf_pos -= size
  return $ T.takeWhile (\c -> c /= chr 0) text
