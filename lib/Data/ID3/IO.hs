module Data.ID3.IO (
  readv1File
) where

import qualified Data.ByteString as BS
import Text.Megaparsec
import Data.ID3.Tag
import Data.ID3.Parse

readv1File :: FilePath -> Handle -> IO (Maybe ID3v1)
readv1File name file = do
  contents <- BS.hGetContents file
  return $ findv1Tag (toText name) contents

findv1Tag :: Text -> ByteString -> Maybe ID3v1
findv1Tag name contents =
  let v10len = 128
      v1Elen = v10len + 227
      v12len = v10len + 128
  in let (_data1E, tag1E) = BS.splitAt (BS.length contents - v1Elen) contents
    in if BS.take 4 tag1E == encodeUtf8 ("TAG+" :: Text)
      then ID3v1E <$> runTagParser_ parseID3v1ETag name tag1E
  else let (_data12, tag12) = BS.splitAt (BS.length contents - v12len) contents
    in if BS.take 3 tag12 == encodeUtf8 ("EXT" :: Text)
      then ID3v12 <$> runTagParser_ parseID3v12Tag name tag12
  else let (_data1x, tag1x) = BS.splitAt (BS.length contents - v10len) contents
    in if BS.take 3 tag1x == encodeUtf8 ("TAG" :: Text)
      then ID3v1x <$> runTagParser_ parseID3v1xTag name tag1x
  else Nothing
