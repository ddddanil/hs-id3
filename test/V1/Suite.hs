module V1.Suite
  ( getSuite
  , fileList
  , testV1Suite
  )
  where

import Control.Exception
import Control.Lens
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified System.Directory as Dir
import System.IO (openBinaryFile)
import qualified Path as P
import Network.HTTP.Req

import Data.ID3.V1.Tag
import Data.ID3.Parse

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)

testV1Suite :: IO TestTree
testV1Suite =
  Dir.withCurrentDirectory suitePath $ do
    test_files <- Dir.listDirectory "."
    return . testGroup "id3v1 Suite" $
      testFile <$> test_files

testFile :: FilePath -> TestTree
testFile filename =
  testCase filename .
  Dir.withCurrentDirectory suitePath $ do
    let fail = "_F" `L.isInfixOf` filename
    file <- openBinaryFile filename ReadMode
    contents <- BS.hGetContents file
    case runTagParser_ parseID3v1 filename contents of
      Just (ParseResult _ (Just t)) -> do
        writeFileText (filename ++ ".tag") . show $ t
        when fail $ assertFailure "Correct tag"
      Just (ParseResult _ Nothing) -> unless fail $ assertFailure "Incorrect tag"
      Nothing -> assertFailure "Parser error"

-- List files

fileList :: IO [FilePath]
fileList = do
  exists <- Dir.doesDirectoryExist suitePath
  if exists
    then Dir.listDirectory suitePath
    else return []

-- Download and extract suite

suitePath :: FilePath
suitePath = "id3v1_test_suite"

download :: IO LBS.ByteString
download = do
-- https://id3.org/Developer%20Information?action=AttachFile&do=get&target=id3v1_test_suite.tar.gz
  runReq defaultHttpConfig $
    responseBody <$> req
      GET (https "id3.org" /: "Developer Information")
      NoReqBody lbsResponse
      (("action" :: Text) =: ("AttachFile" :: Text) <> ("do" :: Text) =: ("get" :: Text) <> ("target" :: Text) =: ("id3v1_test_suite.tar.gz" :: Text))

extractEntries :: Tar.Entries Tar.FormatError -> IO [Tar.Entry]
extractEntries Tar.Done = return []
extractEntries (Tar.Fail e) = throwIO e
extractEntries (Tar.Next entry rest) = (entry :) <$> extractEntries rest

writeTestEntry :: Tar.Entry -> IO ()
writeTestEntry entry =
  case Tar.entryContent entry of
    Tar.NormalFile contents _size -> do
      name <- P.fromRelFile . P.filename <$> (P.parseRelFile . Tar.entryPath $ entry)
      when (".mp3" `L.isInfixOf` name) $ do
        putStrLn $ "\tExtracting \"" ++ name ++ "\""
        writeFileLBS name contents
    _ -> pass

extract :: LBS.ByteString -> IO ()
extract archive= do
  putText "\tDecompressing gzip archive"
  entries <- extractEntries . Tar.read . GZip.decompress $ archive
  Dir.withCurrentDirectory suitePath $
    mapM_ writeTestEntry entries

getSuite :: IO ()
getSuite = do
  exists <- Dir.doesDirectoryExist suitePath
  unless exists $ do
    Dir.createDirectory suitePath
    putText "==> Downloading id3v1 test suite"
    archive <- download
    putStrLn $ "\tDownloaded [" ++ show (LBS.length archive `div` 1024) ++ " kB]"
    putText "==> Extracting id3v1 test suite"
    extract archive
