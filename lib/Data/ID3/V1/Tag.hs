module Data.ID3.V1.Tag where

import Control.Lens
import Control.Lens.TH
import Data.Generics.Product.Fields
import Data.ID3.V1.Tag.V10
import Data.ID3.V1.Tag.V11
import Data.ID3.V1.Tag.V12
import Data.ID3.V1.Tag.V1E


data ID3v1Tag
  = ID3v10 !ID3v10Tag
  | ID3v11 !ID3v11Tag
  | ID3v12 !ID3v12Tag
  | ID3v1E !ID3v1ETag
  deriving (Eq, Show, Generic)
