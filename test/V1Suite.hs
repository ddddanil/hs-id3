module V1Suite
  ( getSuite
  , fileList
  , testV1Suite
  )
  where

import Protolude
import qualified Data.List as L
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified System.Directory as Dir
import qualified Path as P
import Network.HTTP.Req

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

testV1Suite :: TestTree
testV1Suite = testGroup "id3v1 Suite"
  [ testCase "test2" $ return ()
  ]



-- List files

fileList :: IO [FilePath]
fileList =
  Dir.withCurrentDirectory "test" $ do
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
        LBS.writeFile name contents
    _ -> return ()

extract :: LBS.ByteString -> IO ()
extract archive= do
  putText "\tDecompressing gzip archive"
  entries <- extractEntries . Tar.read . GZip.decompress $ archive
  Dir.withCurrentDirectory suitePath $
    mapM_ writeTestEntry entries

getSuite :: IO ()
getSuite =
  Dir.withCurrentDirectory "test" $ do
    exists <- Dir.doesDirectoryExist suitePath
    unless exists $ do
      Dir.createDirectory suitePath
      putText "==> Downloading id3v1 test suite"
      archive <- download
      putStrLn $ "\tDownloaded [" ++ show (LBS.length archive `div` 1024) ++ " kB]"
      putText "==> Extracting id3v1 test suite"
      extract archive
