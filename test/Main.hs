module Main where

import qualified System.Directory as Dir
import Test.Tasty       (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

-- import qualified V1Suite
import V1.Lenses

-- main :: IO ()
-- main = 
--   Dir.withCurrentDirectory "test" $ do
--     V1Suite.getSuite
--     testv1suite <- V1Suite.testV1Suite
--     defaultMain $
--       testGroup "Tests"
--       [ testv1suite
--       ]

main :: IO ()
main = defaultMain $
  testGroup "Lens Laws"
    [ checkv1Lenses
    ]
