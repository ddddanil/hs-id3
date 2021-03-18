module Main where

import Protolude
import Test.Tasty       (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

import qualified V1Suite

main :: IO ()
main = do
  V1Suite.getSuite
  defaultMain $
    testGroup "Tests"
    [ testCase "test" $ return ()
    , V1Suite.testV1Suite
    ]