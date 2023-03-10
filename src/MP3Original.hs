module MP3Original
  ( duration
  ) where

import           Data.Bifunctor (bimap)
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Functor
import           Data.List (foldl1')
import qualified Data.List.NonEmpty as NE
import           Data.Monoid
import           Data.Void
import           Data.Word
import           Numeric (showHex)
import           Text.Megaparsec
import           Text.Megaparsec.Byte

type Parser = Parsec Void BS.ByteString

newtype Frame = Frame SampleRate

-- | Returns error text and position
errorP :: (VisualStream s, ShowErrorComponent e) => ParseErrorBundle s e -> (String, Int)
errorP e = let
    pe = NE.head $ bundleErrors e
    pos = case pe of
      TrivialError pos' _ _ -> pos'
      FancyError pos' _ -> pos'
  in (parseErrorTextPretty pe, pos)

-- | Parses an MP3 frame header, assuming MPEG-1 Layer 3 and
-- without error protection.
mp3Frame :: Parser Frame
mp3Frame = do
  _ <- string "\xff\xfb" <?> "frame start"
  details <- anySingle
  void anySingle

  let (skipLength, sampleRate) = restLength details
  skipCount skipLength anySingle
  return $ Frame sampleRate

  where
    restLength :: Word8 -> (Int, SampleRate)
    restLength details =
      let br = BitRate . (bitRates !!) . (.&. 0x0f) . (`shiftR` 4) . fromIntegral $ details
          sr = SampleRate . (sampleRates !!) . (.&. 0x03) . (`shiftR` 2) . fromIntegral $ details
          p = Padding $ testBit details 1

          bitRates =
            [ undefined, 32000, 40000, 48000
            , 56000, 64000, 80000, 96000
            , 112000, 128000, 160000, 192000
            , 224000, 256000, 320000, undefined
            ]
          sampleRates = [44100, 48000, 32000, undefined]
      in (frameLen br sr p - 4, sr)

-- | Parses an MP3 stream.
mp3Parser :: Parser [Frame]
mp3Parser = do
  _ <- optional id3Parser
  frames <- some mp3Frame
  _ <- optional id3v1
  eof

  pure frames

  where
    id3Parser = do
      _ <- string "ID3"
      _ <- (char 0x03 <|> char 0x04) *> char 0x00 <?> "version"
      _ <- char 0x00 <?> "flags"
      rawSize <- count 4 (satisfy msbIsZero)
      skipCount (unpackSize rawSize) anySingle

    id3v1 = do
      _ <- string "TAG" <?> "ID3v1 start"
      skipCount (128 - 3) anySingle

    -- https://id3.org/id3v2.4.0-structure
    msbIsZero = (< 0x80) -- flip testBit 7

    unpackSize :: [Word8] -> Int
    unpackSize bytes =
      let words32 = fromInteger . toInteger <$> bytes :: [Word32]
          shifted =
            [ head words32 `shiftL` 21
            , words32 !! 1 `shiftL` 14
            , words32 !! 2 `shiftL` 7
            , words32 !! 3
            ]
      in fromIntegral $ foldl1' (.|.) shifted

-- | Parses the MP3 data and returns the stream's duration in seconds.
duration :: BS.ByteString -> Either String Double
duration = bimap
  ((\(e, pos) -> mconcat [e, " at byte 0x", showHex pos ""]) . errorP)
  (getSum . foldMap (Sum . frameDuration))
  . parse mp3Parser ""
  where
    frameDuration (Frame f) = numSamplesInFrame / fromIntegral (unSampleRate f)
    numSamplesInFrame = 1152

newtype BitRate = BitRate { unBitRate :: Int }
newtype SampleRate = SampleRate { unSampleRate :: Int }
newtype Padding = Padding { unPadding :: Bool }

-- http://www.multiweb.cz/twoinches/MP3inside.htm

-- | @frameLen@ includes frame header (4 bytes).
frameLen :: BitRate -> SampleRate -> Padding -> Int
frameLen br sr p = floor $ (144.0 * (fromIntegral (unBitRate br) :: Double) / (fromIntegral (unSampleRate sr) :: Double)) + padding
  where padding = if unPadding p then 1 else 0
