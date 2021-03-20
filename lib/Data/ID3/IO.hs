module Data.ID3.IO where

import qualified Data.ByteString as BS
import Text.Megaparsec
import Data.ID3.Tag
import Data.ID3.Parse

readv1File :: FilePath -> Handle -> IO (Maybe ID3v1Tag)
readv1File name file = do
  contents <- BS.hGetContents file
  let parsed = runTagParser parseID3v1Tag (toText name) contents
  case parsed of
    Left err -> do
      putStr $ errorBundlePretty err
      return Nothing
    Right tag -> do 
      print tag
      return . Just $ tag
