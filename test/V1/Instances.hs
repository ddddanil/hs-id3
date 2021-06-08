module V1.Instances where

import Control.Lens
import Data.Generics.Product
import Test.QuickCheck
import Test.QuickCheck.Instances.Text
import qualified Data.Text as T
import Data.ID3.V1.Genre as G
import Data.ID3.V1.Tag.V10
import Data.ID3.V1.Tag.V11
import Data.ID3.V1.Tag.V12
import Data.ID3.V1.Tag.V1E
import Data.ID3.V1.Tag

instance Arbitrary Genre where
  arbitrary = chooseInt (0, 190) `suchThatMap` G.genre

instance CoArbitrary Genre
instance Function Genre

instance Arbitrary ID3v10Tag where
  arbitrary = do
    title <- arbitrary @Text `suchThat` ((<= 30) . T.length)
    album <- arbitrary @Text `suchThat` ((<= 30) . T.length)
    artist <- arbitrary @Text `suchThat` ((<= 30) . T.length)
    year <- chooseBoundedIntegral (1980, 2100)
    comment <- arbitrary @Text `suchThat` ((<= 30) . T.length)
    genre <- arbitrary @Genre
    return $ ID3v10Tag title album artist year comment genre

instance CoArbitrary ID3v10Tag
instance Function ID3v10Tag

instance Arbitrary ID3v11Tag where
  arbitrary = do
    title <- arbitrary @Text `suchThat` ((<= 30) . T.length)
    album <- arbitrary @Text `suchThat` ((<= 30) . T.length)
    artist <- arbitrary @Text `suchThat` ((<= 30) . T.length)
    year <- chooseBoundedIntegral (1980, 2100)
    comment <- arbitrary @Text `suchThat` ((<= 28) . T.length)
    track <- chooseAny @Word8
    genre <- arbitrary @Genre
    return $ ID3v11Tag (ID3v10Tag title album artist year comment genre) track

instance CoArbitrary ID3v11Tag
instance Function ID3v11Tag

instance Arbitrary ID3v12Tag where
  arbitrary = do
    v11tag <- arbitrary @ID3v11Tag
    v11tag <- arbitrary @Text `suchThat` ((<= 60) . T.length)
        <&> flip (set (the @"v10tag" . the @"title")) v11tag
    v11tag <- arbitrary @Text `suchThat` ((<= 60) . T.length)
        <&> flip (set (the @"v10tag" . the @"album")) v11tag
    v11tag <- arbitrary @Text `suchThat` ((<= 60) . T.length)
        <&> flip (set (the @"v10tag" . the @"artist")) v11tag
    v11tag <- arbitrary @Text `suchThat` ((<= 43) . T.length)
        <&> flip (set (the @"v10tag" . the @"comment")) v11tag
    subgenre <- arbitrary @Text `suchThat` ((<= 20) . T.length)
    return $ ID3v12Tag v11tag subgenre

instance CoArbitrary ID3v12Tag
instance Function ID3v12Tag

instance Arbitrary TagTime where
  arbitrary = do
    minutes <- chooseBoundedIntegral (0, 999)
    seconds <- chooseBoundedIntegral (0, 99)
    return $ TagTime minutes seconds

instance CoArbitrary TagTime
instance Function TagTime

instance Arbitrary ID3v1ETag where
  arbitrary = do
    v11tag <- arbitrary @ID3v11Tag
    v11tag <- arbitrary @Text `suchThat` ((<= 60) . T.length)
        <&> flip (set (the @"v10tag" . the @"title")) v11tag
    v11tag <- arbitrary @Text `suchThat` ((<= 60) . T.length)
        <&> flip (set (the @"v10tag" . the @"album")) v11tag
    v11tag <- arbitrary @Text `suchThat` ((<= 60) . T.length)
        <&> flip (set (the @"v10tag" . the @"artist")) v11tag
    speed <- chooseBoundedIntegral (0, 4)
    startTime <- arbitrary @TagTime
    endTime <- arbitrary @TagTime
    subgenre <- arbitrary @Text `suchThat` ((<= 30) . T.length)
    return $ ID3v1ETag v11tag speed subgenre startTime endTime

instance CoArbitrary ID3v1ETag
instance Function ID3v1ETag

instance Arbitrary ID3v1Tag where
  arbitrary = do
    let v10 = (_ID3v10 #) <$> arbitrary @ID3v10Tag
    let v11 = (_ID3v11 #) <$> arbitrary @ID3v11Tag
    let v12 = (_ID3v12 #) <$> arbitrary @ID3v12Tag
    let v1E = (_ID3v1E #) <$> arbitrary @ID3v1ETag
    oneof [v10, v11, v12, v1E]

instance CoArbitrary ID3v1Tag
instance Function ID3v1Tag

