module MP3
  ( frameParser
  ) where

import Data.Attoparsec.ByteString ((<?>), Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Bits
import Data.Word

frameParser :: Parser ()
frameParser = do
  _ <- A.string "\xff\xfb" <?> "first two header bytes"
  byte2 <- A.anyWord8
  _ <- A.anyWord8

  samplingRate <- samplingRateParser byte2
  let paddingSize = if testBit byte2 paddingBitIndex then 1 else 0
      contentsSize = frameSize samplingRate - 4 + paddingSize

  _ <- A.take contentsSize
  pure ()

-- | Sampling rate of a frame; it's required to calculate the frame length.
data SamplingRate = SR32000Hz | SR44100Hz | SR48000Hz

-- | Parses the sample rate from the frame byte.
samplingRateParser :: Word8 -> Parser SamplingRate
samplingRateParser byte = case 0b00000011 .&. shiftR byte 2 of
  0b00 -> pure SR44100Hz
  0b01 -> pure SR48000Hz
  0b10 -> pure SR32000Hz
  0b11 -> fail "Unexpected sampling rate \"reserved\" (3)"
  x -> fail $ "Impossible sampling rate value " <> show x

-- | Returns the frame length based on 128 kb/s bitrate and the provided sample rate.
frameSize :: SamplingRate -> Int
frameSize sr = floor @Float $ 144 * 128000 / samplingRateHz sr
  where
    samplingRateHz SR32000Hz = 32000
    samplingRateHz SR44100Hz = 44100
    samplingRateHz SR48000Hz = 48000

-- TODO try https://github.com/stevana/bits-and-bobs
paddingBitIndex :: Int
paddingBitIndex = 1
