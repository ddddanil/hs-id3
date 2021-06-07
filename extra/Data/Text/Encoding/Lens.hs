module Data.Text.Encoding.Lens where

import Control.Lens
import Data.ByteString.Lens
import Data.Bits.Lens
import Data.Text.Lens
import Data.Text.Encoding as E

data Encoding
  = Latin1
  | Utf8
  | Utf16LE
  | Utf16BE
  | Utf32LE
  | Utf32BE
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

encodeLatin1 :: Text -> ByteString
encodeLatin1 = concatMapOf (text . from enum) (concatOf $ partsOf bytewise . to (takeWhile (/= 0))) <&> (^.packedBytes)

decode :: Encoding -> (ByteString -> Text)
decode Latin1  = decodeLatin1
decode Utf8    = E.decodeUtf8
decode Utf16LE = decodeUtf16LE
decode Utf16BE = decodeUtf16BE
decode Utf32LE = decodeUtf32LE
decode Utf32BE = decodeUtf32BE

encode :: Encoding -> (Text -> ByteString)
encode Latin1  = encodeLatin1
encode Utf8    = E.encodeUtf8
encode Utf16LE = encodeUtf16LE
encode Utf16BE = encodeUtf16BE
encode Utf32LE = encodeUtf32LE
encode Utf32BE = encodeUtf32BE

encoded :: Encoding -> Iso' Text ByteString
encoded e = iso (encode e) (decode e)

decoded :: Encoding -> Iso' ByteString Text
decoded e = from $ encoded e
