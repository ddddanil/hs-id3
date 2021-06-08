{-# LANGUAGE UndecidableInstances #-}
module Data.ID3.V1.Genre (
  Genre
, genre
, _Genre
, genreRepr
, isExtended
) where

import Control.Lens
import Data.Text.Prettyprint.Doc
import qualified Data.ByteString.Builder as B
import Text.Megaparsec
import Data.ID3.ReadWrite
import Data.ID3.Parse

newtype Genre = Genre Word8
  deriving (Show, Generic)
  deriving newtype (Eq, Ord, Read, Bounded, Enum)

genre :: Int -> Maybe Genre
genre x
  | x < 0 = Nothing
  | x > 191 = Nothing
  | otherwise = Just . Genre . toEnum $ x

_Genre :: Prism' Word8 Genre
_Genre = prism' coerce (genre . fromIntegral)

genreRepr :: Getter Genre Word8
genreRepr = to coerce

isExtended :: Getter Genre Bool
isExtended = to $ \(Genre x) -> x >= 80


-- Read & Write

parseGenre :: Parser Genre
parseGenre = do
  g <- anySingle
  guard (g <= 191) <?> "valid genre byte"
  return $ Genre g

writeGenre :: Genre -> B.Builder 
writeGenre (Genre g) = B.word8 g

instance ReadWrite Genre where
  parse = parseGenre
  write = writeGenre


-- Pretty

genreName :: Genre -> Text
genreName (Genre x) = case x of
  0   -> "Blues"
  1   -> "Classic Rock"
  2   -> "Country"
  3   -> "Dance"
  4   -> "Disco"
  5   -> "Funk"
  6   -> "Grunge"
  7   -> "Hip-Hop"
  8   -> "Jazz"
  9   -> "Metal"
  10  -> "New Age"
  11  -> "Oldies"
  12  -> "Other"
  13  -> "Pop"
  14  -> "Rhythm and Blues"
  15  -> "Rap"
  16  -> "Reggae"
  17  -> "Rock"
  18  -> "Techno"
  19  -> "Industrial"
  20  -> "Alternative"
  21  -> "Ska"
  22  -> "Death Metal"
  23  -> "Pranks"
  24  -> "Soundtrack"
  25  -> "Ambient"
  26  -> "Euro-Techno"
  27  -> "Trip-Hop"
  28  -> "Vocal"
  29  -> "Jazz & Funk"
  30  -> "Fusion"
  31  -> "Trance"
  32  -> "Classical"
  33  -> "Instrumental"
  34  -> "Acid"
  35  -> "House"
  36  -> "Game"
  37  -> "Sound clip"
  38  -> "Gospel"
  39  -> "Noise"
  40  -> "Alternative Rock"
  41  -> "Bass"
  42  -> "Soul"
  43  -> "Punk"
  44  -> "Space"
  45  -> "Meditative"
  46  -> "Instrumental Pop"
  47  -> "Instrumental Rock"
  48  -> "Ethnic"
  49  -> "Gothic"
  50  -> "Darkwave"
  51  -> "Techno-Industrial"
  52  -> "Electronic"
  53  -> "Pop-Folk"
  54  -> "Eurodance"
  55  -> "Dream"
  56  -> "Southern Rock"
  57  -> "Comedy"
  58  -> "Cult"
  59  -> "Gangsta"
  60  -> "Top 40"
  61  -> "Christian Rap"
  62  -> "Pop/Funk"
  63  -> "Jungle"
  64  -> "Native US"
  65  -> "Cabaret"
  66  -> "New Wave"
  67  -> "Psychedelic"
  68  -> "Rave"
  69  -> "Show tunes"
  70  -> "Trailer"
  71  -> "Lo-Fi"
  72  -> "Tribal"
  73  -> "Acid Punk"
  74  -> "Acid Jazz"
  75  -> "Polka"
  76  -> "Retro"
  77  -> "Musical"
  78  -> "Rock 'n' Roll"
  79  -> "Hard rock"
  80  -> "Folk"
  81  -> "Folk-Rock"
  82  -> "National Folk"
  83  -> "Swing"
  84  -> "Fast Fusion"
  85  -> "Bebop"
  86  -> "Latin"
  87  -> "Revival"
  88  -> "Celtic"
  89  -> "Bluegrass"
  90  -> "Avantgarde"
  91  -> "Gothic Rock"
  92  -> "Progressive Rock"
  93  -> "Psychodelic Rock"
  94  -> "Symphonic Rock"
  95  -> ""
  96  -> ""
  97  -> ""
  98  -> ""
  99  -> ""
  100 -> ""
  101 -> ""
  102 -> ""
  103 -> ""
  104 -> ""
  105 -> ""
  106 -> ""
  107 -> ""
  108 -> ""
  109 -> ""
  110 -> ""
  111 -> ""
  112 -> ""
  113 -> ""
  114 -> ""
  115 -> ""
  116 -> ""
  117 -> ""
  118 -> ""
  119 -> ""
  120 -> ""
  121 -> ""
  122 -> ""
  123 -> ""
  124 -> ""
  125 -> ""
  126 -> ""
  127 -> ""
  128 -> ""
  129 -> ""
  130 -> ""
  131 -> ""
  132 -> ""
  133 -> ""
  134 -> ""
  135 -> ""
  136 -> ""
  137 -> ""
  138 -> ""
  139 -> ""
  140 -> ""
  141 -> ""
  142 -> ""
  143 -> ""
  144 -> ""
  145 -> ""
  146 -> ""
  147 -> ""
  148 -> ""
  149 -> ""
  150 -> ""
  151 -> ""
  152 -> ""
  153 -> ""
  154 -> ""
  155 -> ""
  156 -> ""
  157 -> ""
  158 -> ""
  159 -> ""
  160 -> ""
  161 -> ""
  162 -> ""
  163 -> ""
  164 -> ""
  165 -> ""
  166 -> ""
  167 -> ""
  168 -> ""
  169 -> ""
  170 -> ""
  171 -> ""
  172 -> ""
  173 -> ""
  174 -> ""
  175 -> ""
  176 -> ""
  177 -> ""
  178 -> ""
  179 -> ""
  180 -> ""
  181 -> ""
  182 -> ""
  183 -> ""
  184 -> ""
  185 -> ""
  186 -> ""
  187 -> ""
  188 -> ""
  189 -> ""
  190 -> ""
  191 -> ""
  _ -> error "Illegal genre"

instance Pretty Genre where
  pretty = pretty . genreName
    