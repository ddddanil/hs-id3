module Data.ID3.IO where

import Protolude
import qualified Data.String as S
import qualified Data.ByteString as BS
import Text.Megaparsec
import Data.ID3.Tag
import Data.ID3.Parse

parseFromFile p file = runTagParser p <$> BS.readFile file

readv1File :: S.String -> IO (Maybe ID3v1Tag)
readv1File f = do
  parsed <- parseFromFile (parseID3v1Tag) f
  case parsed of
    Left err -> do
      putStr $ errorBundlePretty err
      return Nothing
    Right tag -> do 
      print tag
      return . Just $ tag
