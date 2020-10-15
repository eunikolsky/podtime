{-# LANGUAGE OverloadedStrings #-}

module MP3 where

import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import           Data.Functor
import           Data.List (foldl1', genericLength)
import           Data.Void
import           Data.Word
import           Text.Megaparsec
import           Text.Megaparsec.Byte

type Parser = Parsec Void BL.ByteString

type ParserBL = Parser BL.ByteString

type Frame = ()

-- | Parses an MP3 frame header, assuming MPEG-1 Layer 3 and
-- without error protection.
mp3Frame :: Parser Frame
mp3Frame = do
  void $ string "\xff\xfb"
  details <- anySingle
  void anySingle

  skipCount (restLength details) anySingle
  return ()

  where
    restLength :: Word8 -> Int
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
      in (frameLen br sr p) - 4

-- | Parses an MP3 stream.
mp3Parser :: Parser [Frame]
mp3Parser = do
  optional id3Parser
  some mp3Frame <* optional id3v1

  where
    id3Parser = do
      string "ID3"
      (char 0x03 <|> char 0x04) *> char 0x00 <?> "version"
      char 0x00 <?> "flags"
      rawSize <- count 4 (satisfy msbIsZero)
      skipCount (unpackSize rawSize) anySingle

    id3v1 = do
      string "TAG"
      skipCount (128 - 3) anySingle

    -- https://id3.org/id3v2.4.0-structure
    msbIsZero = (< 0x80) -- flip testBit 7

    unpackSize :: [Word8] -> Int
    unpackSize words =
      let words32 = fromInteger . toInteger <$> words :: [Word32]
          shifted =
            [ words32 !! 0 `shiftL` 21
            , words32 !! 1 `shiftL` 14
            , words32 !! 2 `shiftL` 7
            , words32 !! 3
            ]
      in fromIntegral $ foldl1' (.|.) shifted

-- | Parses the MP3 data and returns the stream's duration in seconds.
duration :: BL.ByteString -> Maybe Double
duration = fmap ((* 0.026) . genericLength) . parseMaybe mp3Parser

newtype BitRate = BitRate { unBitRate :: Int }
newtype SampleRate = SampleRate { unSampleRate :: Int }
newtype Padding = Padding { unPadding :: Bool }

-- http://www.multiweb.cz/twoinches/MP3inside.htm

-- | @frameLen@ includes frame header (4 bytes).
frameLen :: BitRate -> SampleRate -> Padding -> Int
frameLen br sr p = floor $ (144.0 * (fromIntegral (unBitRate br) :: Double) / (fromIntegral (unSampleRate sr) :: Double)) + padding
  where padding = if unPadding p then 1 else 0
