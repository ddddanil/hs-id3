module V1.Lenses where

import Control.Lens.Properties
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.ID3.V1.Tag as V1
import V1.Instances

checkv1Lenses :: TestTree
checkv1Lenses = testProperties "v1 Lenses"
  [ ("title", isLens V1.title)
  , ("album", isLens V1.album)
  , ("artist", isLens V1.artist)
  , ("comment", isLens V1.comment)
  , ("year", isLens V1.year)
  , ("genre", isLens V1.genre)
  , ("track", isTraversal V1.track)
  , ("subgenre", isTraversal V1.subgenre)
  ]
