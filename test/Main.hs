module Main where

import qualified System.Directory as Dir
import Test.Tasty       (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

import V1.Suite
import V1.Lenses

main :: IO ()
main = 
  Dir.withCurrentDirectory "test" $ do
    getSuite
    testv1suite <- testV1Suite
    defaultMain $
      testGroup "Tests"
      [ testv1suite
      , checkv1Lenses
      ]
