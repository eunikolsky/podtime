module MP3
  ( AudioDuration(..)
  , frameParser
  , mp3Parser
  ) where

import AttoparsecExtra
import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString ((<?>), Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.Combinator qualified as A (lookAhead)
import Data.Bits
import Data.ByteString.Builder qualified as BSB
import Data.List (singleton)
import Data.Word
import ID3 qualified as ID3V2
import ID3V1 qualified
import Text.Printf

-- | Duration of an MP3 file, in seconds.
newtype AudioDuration = AudioDuration { getAudioDuration :: Float }
  deriving newtype (Eq, Ord, Fractional, Num)

instance Show AudioDuration where
  show (AudioDuration d) = show d <> " s"

-- | Parses an MP3 file and returns the audio duration. An accepted MP3 file:
--
-- - optionally starts with:
--   - an ID3 v2.{2,3,4} tag, which may be followed by a single space or a
--   block of null bytes;
--   - or a leftover from a previous frame [0][1];
--
-- - consists of 1+ MP3 frames, where a frame may be followed by an optional
-- null byte;
--
-- - optionally ends with:
--   - an ID3 v1 tag;
--   - or a piece of the next frame [0][2].
--
-- [0] From parser's point of view, these pieces are junk. They appear when you
-- dump an internet MP3 stream connecting at an arbitrary point in time. In a
-- valid MP3 stream, the junk must be shorter than the longest frame's size
-- (1440 bytes). I don't think ID3 tags are present in such streams (ICY
-- metadata can be used instead).
--
-- [1] It can include bytes that look like a valid frame start (`fffb` + 2 bytes)
-- and not be one. To validate a frame start, we have to read the possible frame
-- and check whether the next frame is located right after this one because it is
-- very unlikely that the stream will have random `fffb`s at the correct
-- spacings. That is, it makes sense to require two sequential valid frames to
-- filter out junk correctly.
--
-- [2] I suppose if a stream ends successfully, the server (such as `icecast`)
-- should send the complete last frame. However in case of a disconnect, you're
-- likely to receive a partial frame. The parser expects to read a valid frame
-- header and then any truncated data (if the frame isn't truncated, it was
-- already parsed by the frame parsing loop above); if the junk doesn't start
-- with a complete frame header (4 bytes), parsing fails; the parser could
-- accept any junk, but then it would be too generic, could fail somewhere in the
-- middle of a file and hide more specific errors.
mp3Parser :: Parser AudioDuration
mp3Parser = do
  _ <- optional $ do
    ID3V2.id3Parser
    -- even though this padding is only skipped if it's after ID3, it's
    -- technically not a part of it, that's why it's not defined in `id3Parser`
    skipPostID3Padding

  firstSamplingRates <- allowingPostNullByte parseFirstFrames
  samplingRates <- A.many' $ allowingPostNullByte frameParser

  ID3V1.id3Parser <|>
    -- if we're here, all the sequential valid MP3 frames have been parsed and
    -- there is no ID3 v1 tag, so try parsing the last, truncated frame if any;
    -- it must start with a valid frame header, or the parser fails
    (void . optional $ frameHeaderParser >> A.takeLazyByteString)
  endOfInput
  pure . sum $ frameDuration <$> firstSamplingRates <> samplingRates

-- | Combinator to allow an optional null byte after the given parser. It's used
-- to parse MP3 frames with a possible extra null byte after a frame; several
-- older episodes of "Accidental Tech Podcast" and "Under the Radar" have this
-- byte in addition to the already present padding.
allowingPostNullByte :: Parser a -> Parser a
allowingPostNullByte = (<* optional (A.word8 0))

-- | Parses first MP3 frames of a file skipping any leftovers from a previous
-- frame. It can return either one frame (for valid MP3 files), or two frames
-- (for MP3 stream dumps that don't start with a frame). If the parser has
-- skipped more than 1440 bytes (the max MP3 frame size) and hasn't found a
-- valid frame header, this is an invalid MP3 stream.
parseFirstFrames :: Parser [SamplingRate]
parseFirstFrames = (singleton <$> frameParser) <|> findFirstFrames (SkippedBytesCount 1)
  where
    maxFrameSize = 1440

    findFirstFrames :: SkippedBytesCount -> Parser [SamplingRate]
    findFirstFrames skippedCount
      | skippedCount >= maxFrameSize = fail "Couldn't find a valid MP3 frame after skipping leading junk"
      | otherwise = do
        -- this skips a single byte to retry frames parsing from the next position
        -- one byte skipping isn't very efficient and may or may not be slow;
        -- however for a valid MP3 file, this junk is limited in size; an
        -- alternative is to skip until a `0xff` byte, which is only a part of a
        -- valid frame header, but that leaks lower-level details (of
        -- `frameParser`) into this higher-level parser
        -- TODO benchmark this and skip to `0xff` if necessary
        void A.anyWord8
        A.count 2 frameParser <|> findFirstFrames (skippedCount + 1)

newtype SkippedBytesCount = SkippedBytesCount Int
  deriving newtype (Num, Eq, Ord)

frameDuration :: SamplingRate -> AudioDuration
frameDuration = AudioDuration . (samplesPerFrame /) . samplingRateHz
  where samplesPerFrame = 1152

-- | Expects an end-of-input. If it fails [1], there is a failure message
-- containing the current position and next 4 bytes — this helps with parser
-- debugging and improvement.
--
-- [1] which may happen when there is junk in between mp3 frames, so the parser
-- takes all the frames before the junk and then expects an EOF
endOfInput :: Parser ()
endOfInput = do
  pos <- getPos
  nextBytes <- A.lookAhead $ takeUpTo 4
  let restDump = show . BSB.byteStringHex $ nextBytes
  A.endOfInput <?>
    printf "Expected end-of-file at byte %#x (%u), but got %s" pos pos restDump

-- | Skips stray bytes that may be present between the end of the ID3 tag and
-- the first frame:
-- * (at least) two episodes of "Cold War Conversations" have 10 null bytes, but
-- it should be safe to skip any number of them because an MP3 frame should
-- start with `0xff` anyway;
-- * multiple "Reply All" and "Darknet Diaries" episodes have a single space
-- character.
--
-- These bytes are outside of the tag size (in the tag header); I couldn't find
-- posts online explaining this issue. `mp3diags` shows them as an "unknown stream".
skipPostID3Padding :: Parser ()
skipPostID3Padding = A.skip (== 0x20) <|> A.skipWhile (== 0)

-- | Parses the header of an MP3 frame and returns its sampling rate and the
-- number of bytes to read for this frame.
frameHeaderParser :: Parser (SamplingRate, Int)
frameHeaderParser = do
  let frameHeaderSize = 4
  [byte0, byte1, byte2, _] <- A.count frameHeaderSize A.anyWord8 <?> "Incomplete frame header"

  frameSyncValidator (byte0, byte1)
  mpegVersionValidator byte1
  layerValidator byte1
  protectionValidator byte1

  bitrate <- bitrateParser byte2
  samplingRate <- samplingRateParser byte2

  let paddingSize = if testBit byte2 paddingBitIndex then 1 else 0
      contentsSize = frameSize bitrate samplingRate - frameHeaderSize + paddingSize

  pure (samplingRate, contentsSize)

-- | Parses a single MP3 frame and returns its sampling rate.
frameParser :: Parser SamplingRate
frameParser = do
  (samplingRate, contentsSize) <- frameHeaderParser
  _ <- A.take contentsSize
  pure samplingRate

-- | Validates that the header bytes contain the valid frame sync.
-- It's called a validator because it returns unit (or error) since we don't
-- care about MPEG Version after this if it's valid.
frameSyncValidator :: (Word8, Word8) -> Parser ()
frameSyncValidator (b0, b1) =
  let isValid = (b0 == 0xff) && (b1 .&. byte1Mask == byte1Mask)
  in unless isValid . fail $ printf "Invalid frame sync (0x%02x%02x, %c%c)" b0 b1 b0 b1
  where byte1Mask = 0b1110_0000

-- | Validates that the header byte declares MPEG Version 1.
mpegVersionValidator :: Word8 -> Parser ()
mpegVersionValidator byte = case 0b00000011 .&. byte `shiftR` 3 of
  0b11 -> pure ()
  0b10 -> fail "Unexpected MPEG version 2 (2) frame"
  0b00 -> fail "Unexpected MPEG version 2.5 (0) frame"
  0b01 -> fail "Unexpected MPEG version \"reserved\" (1) frame"
  x -> fail $ "Impossible MPEG version value " <> show x

-- | Validates that the header byte declares Layer 3.
layerValidator :: Word8 -> Parser ()
layerValidator byte = case 0b0000_0011 .&. byte `shiftR` 1 of
  0b01 -> pure ()
  0b11 -> fail "Unexpected Layer 1 (3) frame"
  0b10 -> fail "Unexpected Layer 2 (2) frame"
  0b00 -> fail "Unexpected Layer \"reserved\" (0) frame"
  x -> fail $ "Impossible Layer value " <> show x

-- | Validates that the header byte doesn't declare the CRC protection.
protectionValidator :: Word8 -> Parser ()
protectionValidator byte = case 0b0000_0001 .&. byte of
  1 -> pure ()
  0 -> fail "Unexpected CRC-protected (0) frame"
  x -> fail $ "Impossible Protection value " <> show x

-- | Sampling rate of a frame; it's required to calculate the frame length.
data SamplingRate = SR32000Hz | SR44100Hz | SR48000Hz

instance Show SamplingRate where
  show SR32000Hz = "32 kHz"
  show SR44100Hz = "44.1 kHz"
  show SR48000Hz = "48 kHz"

-- | Bitrate of a frame; it's required to calculate the frame length.
data Bitrate
  = BR32kbps
  | BR40kbps
  | BR48kbps
  | BR56kbps
  | BR64kbps
  | BR80kbps
  | BR96kbps
  | BR112kbps
  | BR128kbps
  | BR160kbps
  | BR192kbps
  | BR224kbps
  | BR256kbps
  | BR320kbps

-- | Parses the sample rate from the frame byte.
samplingRateParser :: Word8 -> Parser SamplingRate
samplingRateParser byte = case 0b00000011 .&. shiftR byte 2 of
  0b00 -> pure SR44100Hz
  0b01 -> pure SR48000Hz
  0b10 -> pure SR32000Hz
  0b11 -> fail "Unexpected sampling rate \"reserved\" (3)"
  x -> fail $ "Impossible sampling rate value " <> show x

-- | Parses the bitrate from the frame byte.
bitrateParser :: Word8 -> Parser Bitrate
bitrateParser byte = case shiftR byte 4 of
  0b0001 -> pure BR32kbps
  0b0010 -> pure BR40kbps
  0b0011 -> pure BR48kbps
  0b0100 -> pure BR56kbps
  0b0101 -> pure BR64kbps
  0b0110 -> pure BR80kbps
  0b0111 -> pure BR96kbps
  0b1000 -> pure BR112kbps
  0b1001 -> pure BR128kbps
  0b1010 -> pure BR160kbps
  0b1011 -> pure BR192kbps
  0b1100 -> pure BR224kbps
  0b1101 -> pure BR256kbps
  0b1110 -> pure BR320kbps
  0b0000 -> fail "Unexpected bitrate \"free\" (0)"
  0b1111 -> fail "Unexpected bitrate \"bad\" (15)"
  x -> fail $ "Impossible bitrate value " <> show x

-- | Returns the frame length based on 128 kb/s bitrate and the provided sample rate.
frameSize :: Bitrate -> SamplingRate -> Int
frameSize br sr = floor @Float $ 144 * bitrateBitsPerSecond br / samplingRateHz sr
  where
    bitrateBitsPerSecond BR32kbps  = 32000
    bitrateBitsPerSecond BR40kbps  = 40000
    bitrateBitsPerSecond BR48kbps  = 48000
    bitrateBitsPerSecond BR56kbps  = 56000
    bitrateBitsPerSecond BR64kbps  = 64000
    bitrateBitsPerSecond BR80kbps  = 80000
    bitrateBitsPerSecond BR96kbps  = 96000
    bitrateBitsPerSecond BR112kbps = 112000
    bitrateBitsPerSecond BR128kbps = 128000
    bitrateBitsPerSecond BR160kbps = 160000
    bitrateBitsPerSecond BR192kbps = 192000
    bitrateBitsPerSecond BR224kbps = 224000
    bitrateBitsPerSecond BR256kbps = 256000
    bitrateBitsPerSecond BR320kbps = 320000

samplingRateHz :: Num a => SamplingRate -> a
samplingRateHz SR32000Hz = 32000
samplingRateHz SR44100Hz = 44100
samplingRateHz SR48000Hz = 48000

-- TODO try https://github.com/stevana/bits-and-bobs
paddingBitIndex :: Int
paddingBitIndex = 1
