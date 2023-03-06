module MP3Spec where

import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word
import MP3
import Prelude hiding (pred)
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "frameParser" $ do
    describe "examples" $ do
      it "parses a basic 128 kbps frame" $ do
        let frame = mkFrame
        complete frameParser `shouldSucceedOn` frame

      it "fails to parse bytes without correct frame sync" $ do
        let frame = mkFrame `replacingHeadWith` 0x00
        frameParser `shouldFailOn` frame

    describe "properties" $ do
      prop "parses a basic 128 kbps frame with any contents"
        . forAll genFrame $ \frame ->
          complete frameParser `shouldSucceedOn` frame

      prop "parses a basic 128 kbps frame with padding bit"
        . forAll genFrameWithPadding $ \frame ->
          complete frameParser `shouldSucceedOn` frame

      forM_ [SR44100, SR48000, SR32000] $ \samplingRate ->
        prop ("parses a basic 128 kbps frame with sampling rate " <> show samplingRate) .
          forAll (genFrameWithSamplingRate samplingRate) $ \frame ->
            complete frameParser `shouldSucceedOn` frame

      forM_ [minBound..maxBound] $ \bitrate ->
        prop ("parses a " <> show bitrate <> " frame") .
          forAll (genFrameWithBitrate $ BRValid bitrate) $ \frame ->
            complete frameParser `shouldSucceedOn` frame

      prop "fails to parse bytes with incorrect first byte"
        . forAll genInvalidFrame $ \frame ->
          frameParser `shouldFailOn` frame

      prop "fails to parse frame with reserved sampling rate"
        . forAll (genFrameWithSamplingRate SRReserved) $ \frame ->
          case frame ~> frameParser of
            Left err -> err `shouldContain` "Unexpected sampling rate \"reserved\" (3)"
            Right parsed -> expectationFailure $ "parsed frame " <> show parsed

      prop "fails to parse frame with free bitrate"
        . forAll (genFrameWithBitrate BRFree) $ \frame ->
          case frame ~> frameParser of
            Left err -> err `shouldContain` "Unexpected bitrate \"free\" (0)"
            Right parsed -> expectationFailure $ "parsed frame " <> show parsed

      prop "fails to parse frame with bad bitrate"
        . forAll (genFrameWithBitrate BRBad) $ \frame ->
          case frame ~> frameParser of
            Left err -> err `shouldContain` "Unexpected bitrate \"bad\" (15)"
            Right parsed -> expectationFailure $ "parsed frame " <> show parsed

-- | Parser combinator to make sure the entire input is consumed.
complete :: Parser a -> Parser a
complete = (<* A.endOfInput)

frameSize, frameHeaderSize, contentsSize :: Int
frameSize = 417
frameHeaderSize = 4
contentsSize = frameSize - frameHeaderSize

newtype PaddingBit = PaddingBit Bool

data SamplingRate = SR44100 | SR48000 | SR32000 | SRReserved

instance Show SamplingRate where
  show SR44100 = "44.1 kHz"
  show SR48000 = "48 kHz"
  show SR32000 = "32 kHz"
  show SRReserved = "<Reserved>"

-- | Valid bitrate values; they are separate from `Bitrate` in order to generate
-- successful parsing tests for `[minBound..maxBound]`.
data ValidBitrateValue
  = VBV32 | VBV40 | VBV48 | VBV56 | VBV64 | VBV80 | VBV96
  | VBV112 | VBV128 | VBV160 | VBV192 | VBV224 | VBV256 | VBV320
  deriving stock (Bounded, Enum)

data Bitrate = BRValid ValidBitrateValue | BRFree | BRBad

instance Show ValidBitrateValue where
  show VBV32  = "32 kb/s"
  show VBV40  = "40 kb/s"
  show VBV48  = "48 kb/s"
  show VBV56  = "56 kb/s"
  show VBV64  = "64 kb/s"
  show VBV80  = "80 kb/s"
  show VBV96  = "96 kb/s"
  show VBV112 = "112 kb/s"
  show VBV128 = "128 kb/s"
  show VBV160 = "160 kb/s"
  show VBV192 = "192 kb/s"
  show VBV224 = "224 kb/s"
  show VBV256 = "256 kb/s"
  show VBV320 = "320 kb/s"

mkHeader :: PaddingBit -> SamplingRate -> Bitrate -> ByteString
mkHeader (PaddingBit paddingBitSet) sr br = BS.pack
  [ 0xff
  , 0b11111011
  , byte2
  , 0b11000100
  ]

  where
    paddingBitIndex = 1
    byte2 =
      (.|. bitrateByte br)
      . (.|. samplingRateByte sr)
      . flip (if paddingBitSet then setBit else clearBit) paddingBitIndex
      $ zeroBits

-- | Returns a zeroed frame byte where only the sampling rate bits are set
-- corresponding to `SamplingRate`.
samplingRateByte :: SamplingRate -> Word8
samplingRateByte SR44100    = zeroBits
samplingRateByte SR48000    = 0b00000100
samplingRateByte SR32000    = 0b00001000
samplingRateByte SRReserved = 0b00001100

-- | Returns a zeroed frame byte where only the bitrate bits are set
-- corresponding to `Bitrate`.
bitrateByte :: Bitrate -> Word8
bitrateByte = (`shiftL` 4) . byte
  where
    byte (BRValid VBV32)  = 0b0001
    byte (BRValid VBV40)  = 0b0010
    byte (BRValid VBV48)  = 0b0011
    byte (BRValid VBV56)  = 0b0100
    byte (BRValid VBV64)  = 0b0101
    byte (BRValid VBV80)  = 0b0110
    byte (BRValid VBV96)  = 0b0111
    byte (BRValid VBV112) = 0b1000
    byte (BRValid VBV128) = 0b1001
    byte (BRValid VBV160) = 0b1010
    byte (BRValid VBV192) = 0b1011
    byte (BRValid VBV224) = 0b1100
    byte (BRValid VBV256) = 0b1101
    byte (BRValid VBV320) = 0b1110
    byte BRFree           = 0b0000
    byte BRBad            = 0b1111

-- | A standard 128 kb/s, 44.1 kHz mp3 frame header.
header :: ByteString
header = mkHeader (PaddingBit False) SR44100 (BRValid VBV128)

-- | A standard 128 kb/s, 44.1 kHz mp3 frame.
mkFrame :: ByteString
mkFrame = header <> contents
  where contents = BS.replicate contentsSize 0

-- | Generates a standard 128 kb/s, 44.1 kHz mp3 frame with arbitrary contents.
genFrame :: Gen ByteString
genFrame = do
  contents <- vectorOf contentsSize arbitrary
  pure $ header <> BS.pack contents

-- | Generates a standard 128 kb/s, 44.1 kHz mp3 frame with padding bit set and
-- arbitrary contents.
genFrameWithPadding :: Gen ByteString
genFrameWithPadding = do
  contents <- vectorOf (contentsSize + 1) arbitrary
  pure $ mkHeader (PaddingBit True) SR44100 (BRValid VBV128) <> BS.pack contents

-- | Generates a standard 128 kb/s mp3 frame with the given sampling rate and
-- arbitrary contents.
genFrameWithSamplingRate :: SamplingRate -> Gen ByteString
genFrameWithSamplingRate sr = do
  let contentsSize' = case sr of
        SR44100 -> 417
        SR48000 -> 384
        SR32000 -> 576
        SRReserved -> 0
  contents <- vectorOf (contentsSize' - frameHeaderSize `noLessThan` 0) arbitrary
  pure $ mkHeader (PaddingBit False) sr (BRValid VBV128) <> BS.pack contents

{- table of frame lengths:
(32000.0,[144,180,216,252,288,360,432,504,576,720,864,1008,1152,1440])
(44100.0,[104,130,156,182,208,261,313,365,417,522,626,731,835,1044])
(48000.0,[96,120,144,168,192,240,288,336,384,480,576,672,768,960])
 -}

-- | Generates a 44100 Hz mp3 frame with the given bitrate and arbitrary contents.
genFrameWithBitrate :: Bitrate -> Gen ByteString
genFrameWithBitrate br = do
  let contentsSize' = case br of
        (BRValid VBV32)  -> 104
        (BRValid VBV40)  -> 130
        (BRValid VBV48)  -> 156
        (BRValid VBV56)  -> 182
        (BRValid VBV64)  -> 208
        (BRValid VBV80)  -> 261
        (BRValid VBV96)  -> 313
        (BRValid VBV112) -> 365
        (BRValid VBV128) -> 417
        (BRValid VBV160) -> 522
        (BRValid VBV192) -> 626
        (BRValid VBV224) -> 731
        (BRValid VBV256) -> 835
        (BRValid VBV320) -> 1044
        BRFree           -> 0
        BRBad            -> 0
  contents <- vectorOf (contentsSize' - frameHeaderSize `noLessThan` 0) arbitrary
  pure $ mkHeader (PaddingBit False) SR44100 br <> BS.pack contents

-- | Returns the first number if it's >= the second number; otherwise, the second
-- number. It's a more obvious name for `max`.
noLessThan :: Ord a => a -> a -> a
noLessThan = max

-- | Generates an invalid mp3 frame where the first byte is incorrect.
genInvalidFrame :: Gen ByteString
genInvalidFrame = do
  -- `firstByte <- arbitrary; when (firstByte == 0xff) discard` didn't discard
  -- the first `0xff` byte (seed 181316514)

  firstByte <- firstM (/= 0xff) $ repeat arbitrary
  frame <- genFrame
  pure $ frame `replacingHeadWith` firstByte

firstM :: (Monad m, Show a) => (a -> Bool) -> [m a] -> m a
firstM pred (x:xs) = do
  x' <- x
  if pred x' then pure x' else firstM pred xs
firstM _ [] = error "Unexpected empty xs in firstM"

replacingHeadWith :: ByteString -> Word8 -> ByteString
replacingHeadWith bs n = BS.singleton n <> BS.tail bs
