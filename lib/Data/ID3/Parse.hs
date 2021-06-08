module Data.ID3.Parse where

import Control.Lens
import Control.Monad.Combinators
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding.Lens
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Byte

data ParserOpts = ParserOpts
  { _encoding :: Encoding
  }
  deriving (Show, Generic)
makeLenses ''ParserOpts

type Parser = ParsecT Void ByteString (State ParserOpts) -- Add state

runTagParser :: Parser a -> String -> ByteString -> Either (ParseErrorBundle ByteString Void) a
runTagParser p n s = evalState (runParserT p n s) (ParserOpts Utf8)

runTagParser_ :: Parser a -> String -> ByteString -> Maybe a
runTagParser_ p n = rightToMaybe . runTagParser p n

parseString :: Text -> Parser ByteString
parseString s = do
  enc <- use encoding
  string $ s ^. encoded enc

parseTextField :: Int -> Parser Text
parseTextField size = do
  enc <- use encoding
  takeP (Just "character") size
      <&> BS.takeWhile (/= 0)
      <&> decode Utf8

withInput :: MonadParsec e s m => s -> m a -> m a
withInput i m = do
  st <- getParserState
  setInput i
  res <- m
  setParserState st
  return res
