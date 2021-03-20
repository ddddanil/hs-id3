module Main where

import qualified System.Directory as Dir
import Test.Tasty       (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

import qualified V1Suite

main :: IO ()
main = 
  Dir.withCurrentDirectory "test" $ do
    V1Suite.getSuite
    testv1suite <- V1Suite.testV1Suite
    defaultMain $
      testGroup "Tests"
      [ testv1suite
      ]