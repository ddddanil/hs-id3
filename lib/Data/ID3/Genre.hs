module Data.ID3.Genre where

import Protolude
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

newtype Genre = Genre Word8
  deriving (Show, Eq, Ord, Read)

genre :: Int -> Maybe Genre
genre x
  | x < 0 = Nothing
  | x > 191 = Nothing
  | otherwise = Just . Genre . toEnum $ x

isExtended :: Genre -> Bool
isExtended (Genre x) = x >= 80

instance Pretty Genre where
  pretty = \case 
    (Genre 0) -> pretty ("Blues" :: T.Text)
    (Genre 1) -> pretty ("Classic Rock" :: T.Text)
    (Genre 2) -> pretty ("Country" :: T.Text)
    (Genre 3) -> pretty ("Dance" :: T.Text)
    (Genre 4) -> pretty ("Disco" :: T.Text)
    (Genre 5) -> pretty ("Funk" :: T.Text)
    (Genre 6) -> pretty ("Grunge" :: T.Text)
    (Genre 7) -> pretty ("Hip-Hop" :: T.Text)
    (Genre 8) -> pretty ("Jazz" :: T.Text)
    (Genre 9) -> pretty ("Metal" :: T.Text)
    (Genre 10) -> pretty ("New Age" :: T.Text)
    (Genre 11) -> pretty ("Oldies" :: T.Text)
    (Genre 12) -> pretty ("Other" :: T.Text)
    (Genre 13) -> pretty ("Pop" :: T.Text)
    (Genre 14) -> pretty ("Rhythm and Blues" :: T.Text)
    (Genre 15) -> pretty ("Rap" :: T.Text)
    (Genre 16) -> pretty ("Reggae" :: T.Text)
    (Genre 17) -> pretty ("Rock" :: T.Text)
    (Genre 18) -> pretty ("Techno" :: T.Text)
    (Genre 19) -> pretty ("Industrial" :: T.Text)
    (Genre 20) -> pretty ("Alternative" :: T.Text)
    (Genre 21) -> pretty ("Ska" :: T.Text)
    (Genre 22) -> pretty ("Death Metal" :: T.Text)
    (Genre 23) -> pretty ("Pranks" :: T.Text)
    (Genre 24) -> pretty ("Soundtrack" :: T.Text)
    (Genre 25) -> pretty ("Ambient" :: T.Text)
    (Genre 26) -> pretty ("Euro-Techno" :: T.Text)
    (Genre 27) -> pretty ("Trip-Hop" :: T.Text)
    (Genre 28) -> pretty ("Vocal" :: T.Text)
    (Genre 29) -> pretty ("Jazz & Funk" :: T.Text)
    (Genre 30) -> pretty ("Fusion" :: T.Text)
    (Genre 31) -> pretty ("Trance" :: T.Text)
    (Genre 32) -> pretty ("Classical" :: T.Text)
    (Genre 33) -> pretty ("Instrumental" :: T.Text)
    (Genre 34) -> pretty ("Acid" :: T.Text)
    (Genre 35) -> pretty ("House" :: T.Text)
    (Genre 36) -> pretty ("Game" :: T.Text)
    (Genre 37) -> pretty ("Sound clip" :: T.Text)
    (Genre 38) -> pretty ("Gospel" :: T.Text)
    (Genre 39) -> pretty ("Noise" :: T.Text)
    (Genre 40) -> pretty ("Alternative Rock" :: T.Text)
    (Genre 41) -> pretty ("Bass" :: T.Text)
    (Genre 42) -> pretty ("Soul" :: T.Text)
    (Genre 43) -> pretty ("Punk" :: T.Text)
    (Genre 44) -> pretty ("Space" :: T.Text)
    (Genre 45) -> pretty ("Meditative" :: T.Text)
    (Genre 46) -> pretty ("Instrumental Pop" :: T.Text)
    (Genre 47) -> pretty ("Instrumental Rock" :: T.Text)
    (Genre 48) -> pretty ("Ethnic" :: T.Text)
    (Genre 49) -> pretty ("Gothic" :: T.Text)
    (Genre 50) -> pretty ("Darkwave" :: T.Text)
    (Genre 51) -> pretty ("Techno-Industrial" :: T.Text)
    (Genre 52) -> pretty ("Electronic" :: T.Text)
    (Genre 53) -> pretty ("Pop-Folk" :: T.Text)
    (Genre 54) -> pretty ("Eurodance" :: T.Text)
    (Genre 55) -> pretty ("Dream" :: T.Text)
    (Genre 56) -> pretty ("Southern Rock" :: T.Text)
    (Genre 57) -> pretty ("Comedy" :: T.Text)
    (Genre 58) -> pretty ("Cult" :: T.Text)
    (Genre 59) -> pretty ("Gangsta" :: T.Text)
    (Genre 60) -> pretty ("Top 40" :: T.Text)
    (Genre 61) -> pretty ("Christian Rap" :: T.Text)
    (Genre 62) -> pretty ("Pop/Funk" :: T.Text)
    (Genre 63) -> pretty ("Jungle" :: T.Text)
    (Genre 64) -> pretty ("Native US" :: T.Text)
    (Genre 65) -> pretty ("Cabaret" :: T.Text)
    (Genre 66) -> pretty ("New Wave" :: T.Text)
    (Genre 67) -> pretty ("Psychedelic" :: T.Text)
    (Genre 68) -> pretty ("Rave" :: T.Text)
    (Genre 69) -> pretty ("Show tunes" :: T.Text)
    (Genre 70) -> pretty ("Trailer" :: T.Text)
    (Genre 71) -> pretty ("Lo-Fi" :: T.Text)
    (Genre 72) -> pretty ("Tribal" :: T.Text)
    (Genre 73) -> pretty ("Acid Punk" :: T.Text)
    (Genre 74) -> pretty ("Acid Jazz" :: T.Text)
    (Genre 75) -> pretty ("Polka" :: T.Text)
    (Genre 76) -> pretty ("Retro" :: T.Text)
    (Genre 77) -> pretty ("Musical" :: T.Text)
    (Genre 78) -> pretty ("Rock 'n' Roll" :: T.Text)
    (Genre 79) -> pretty ("Hard rock" :: T.Text)
    (Genre 80) -> pretty ("" :: T.Text)
    (Genre 81) -> pretty ("" :: T.Text)
    (Genre 82) -> pretty ("" :: T.Text)
    (Genre 83) -> pretty ("" :: T.Text)
    (Genre 84) -> pretty ("" :: T.Text)
    (Genre 85) -> pretty ("" :: T.Text)
    (Genre 86) -> pretty ("" :: T.Text)
    (Genre 87) -> pretty ("" :: T.Text)
    (Genre 88) -> pretty ("" :: T.Text)
    (Genre 89) -> pretty ("" :: T.Text)
    (Genre 90) -> pretty ("" :: T.Text)
    (Genre 91) -> pretty ("" :: T.Text)
    (Genre 92) -> pretty ("" :: T.Text)
    (Genre 93) -> pretty ("" :: T.Text)
    (Genre 94) -> pretty ("" :: T.Text)
    (Genre 95) -> pretty ("" :: T.Text)
    (Genre 96) -> pretty ("" :: T.Text)
    (Genre 97) -> pretty ("" :: T.Text)
    (Genre 98) -> pretty ("" :: T.Text)
    (Genre 99) -> pretty ("" :: T.Text)
    (Genre 100) -> pretty ("" :: T.Text)
    (Genre 101) -> pretty ("" :: T.Text)
    (Genre 102) -> pretty ("" :: T.Text)
    (Genre 103) -> pretty ("" :: T.Text)
    (Genre 104) -> pretty ("" :: T.Text)
    (Genre 105) -> pretty ("" :: T.Text)
    (Genre 106) -> pretty ("" :: T.Text)
    (Genre 107) -> pretty ("" :: T.Text)
    (Genre 108) -> pretty ("" :: T.Text)
    (Genre 109) -> pretty ("" :: T.Text)
    (Genre 110) -> pretty ("" :: T.Text)
    (Genre 111) -> pretty ("" :: T.Text)
    (Genre 112) -> pretty ("" :: T.Text)
    (Genre 113) -> pretty ("" :: T.Text)
    (Genre 114) -> pretty ("" :: T.Text)
    (Genre 115) -> pretty ("" :: T.Text)
    (Genre 116) -> pretty ("" :: T.Text)
    (Genre 117) -> pretty ("" :: T.Text)
    (Genre 118) -> pretty ("" :: T.Text)
    (Genre 119) -> pretty ("" :: T.Text)
    (Genre 120) -> pretty ("" :: T.Text)
    (Genre 121) -> pretty ("" :: T.Text)
    (Genre 122) -> pretty ("" :: T.Text)
    (Genre 123) -> pretty ("" :: T.Text)
    (Genre 124) -> pretty ("" :: T.Text)
    (Genre 125) -> pretty ("" :: T.Text)
    (Genre 126) -> pretty ("" :: T.Text)
    (Genre 127) -> pretty ("" :: T.Text)
    (Genre 128) -> pretty ("" :: T.Text)
    (Genre 129) -> pretty ("" :: T.Text)
    (Genre 130) -> pretty ("" :: T.Text)
    (Genre 131) -> pretty ("" :: T.Text)
    (Genre 132) -> pretty ("" :: T.Text)
    (Genre 133) -> pretty ("" :: T.Text)
    (Genre 134) -> pretty ("" :: T.Text)
    (Genre 135) -> pretty ("" :: T.Text)
    (Genre 136) -> pretty ("" :: T.Text)
    (Genre 137) -> pretty ("" :: T.Text)
    (Genre 138) -> pretty ("" :: T.Text)
    (Genre 139) -> pretty ("" :: T.Text)
    (Genre 140) -> pretty ("" :: T.Text)
    (Genre 141) -> pretty ("" :: T.Text)
    (Genre 142) -> pretty ("" :: T.Text)
    (Genre 143) -> pretty ("" :: T.Text)
    (Genre 144) -> pretty ("" :: T.Text)
    (Genre 145) -> pretty ("" :: T.Text)
    (Genre 146) -> pretty ("" :: T.Text)
    (Genre 147) -> pretty ("" :: T.Text)
    (Genre 148) -> pretty ("" :: T.Text)
    (Genre 149) -> pretty ("" :: T.Text)
    (Genre 150) -> pretty ("" :: T.Text)
    (Genre 151) -> pretty ("" :: T.Text)
    (Genre 152) -> pretty ("" :: T.Text)
    (Genre 153) -> pretty ("" :: T.Text)
    (Genre 154) -> pretty ("" :: T.Text)
    (Genre 155) -> pretty ("" :: T.Text)
    (Genre 156) -> pretty ("" :: T.Text)
    (Genre 157) -> pretty ("" :: T.Text)
    (Genre 158) -> pretty ("" :: T.Text)
    (Genre 159) -> pretty ("" :: T.Text)
    (Genre 160) -> pretty ("" :: T.Text)
    (Genre 161) -> pretty ("" :: T.Text)
    (Genre 162) -> pretty ("" :: T.Text)
    (Genre 163) -> pretty ("" :: T.Text)
    (Genre 164) -> pretty ("" :: T.Text)
    (Genre 165) -> pretty ("" :: T.Text)
    (Genre 166) -> pretty ("" :: T.Text)
    (Genre 167) -> pretty ("" :: T.Text)
    (Genre 168) -> pretty ("" :: T.Text)
    (Genre 169) -> pretty ("" :: T.Text)
    (Genre 170) -> pretty ("" :: T.Text)
    (Genre 171) -> pretty ("" :: T.Text)
    (Genre 172) -> pretty ("" :: T.Text)
    (Genre 173) -> pretty ("" :: T.Text)
    (Genre 174) -> pretty ("" :: T.Text)
    (Genre 175) -> pretty ("" :: T.Text)
    (Genre 176) -> pretty ("" :: T.Text)
    (Genre 177) -> pretty ("" :: T.Text)
    (Genre 178) -> pretty ("" :: T.Text)
    (Genre 179) -> pretty ("" :: T.Text)
    (Genre 180) -> pretty ("" :: T.Text)
    (Genre 181) -> pretty ("" :: T.Text)
    (Genre 182) -> pretty ("" :: T.Text)
    (Genre 183) -> pretty ("" :: T.Text)
    (Genre 184) -> pretty ("" :: T.Text)
    (Genre 185) -> pretty ("" :: T.Text)
    (Genre 186) -> pretty ("" :: T.Text)
    (Genre 187) -> pretty ("" :: T.Text)
    (Genre 188) -> pretty ("" :: T.Text)
    (Genre 189) -> pretty ("" :: T.Text)
    (Genre 190) -> pretty ("" :: T.Text)
    (Genre 191) -> pretty ("" :: T.Text)
    _ -> pretty ("Unknown" :: T.Text)
