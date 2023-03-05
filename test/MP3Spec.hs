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

      forM_ (enumFromTo minBound maxBound) $ \samplingRate ->
        prop ("parses a basic 128 kbps frame with sampling rate " <> show samplingRate) .
          forAll (genFrameWithSamplingRate samplingRate) $ \frame ->
            complete frameParser `shouldSucceedOn` frame

      prop "fails to parse bytes with incorrect first byte"
        . forAll genInvalidFrame $ \frame ->
          frameParser `shouldFailOn` frame

-- | Parser combinator to make sure the entire input is consumed.
complete :: Parser a -> Parser a
complete = (<* A.endOfInput)

frameSize, frameHeaderSize, contentsSize :: Int
frameSize = 417
frameHeaderSize = 4
contentsSize = frameSize - frameHeaderSize

newtype PaddingBit = PaddingBit Bool

data SamplingRate = SR44100 | SR48000 | SR32000
  deriving stock (Bounded, Enum)

instance Show SamplingRate where
  show SR44100 = "44.1 kHz"
  show SR48000 = "48 kHz"
  show SR32000 = "32 kHz"

mkHeader :: PaddingBit -> SamplingRate -> ByteString
mkHeader (PaddingBit paddingBitSet) sr = BS.pack
  [ 0xff
  , 0b11111011
  , byte2
  , 0b11000100
  ]

  where
    paddingBitIndex = 1
    byte2 =
      (.|. samplingRateByte sr)
      . flip (if paddingBitSet then setBit else clearBit) paddingBitIndex
      $ 0b10010000

-- | Returns a zeroed frame byte where only the sampling rate bits are set
-- corresponding to `sr`.
samplingRateByte :: SamplingRate -> Word8
samplingRateByte SR44100 = zeroBits
samplingRateByte SR48000 = 0b00000100
samplingRateByte SR32000 = 0b00001000

header :: ByteString
header = mkHeader (PaddingBit False) SR44100

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
  pure $ mkHeader (PaddingBit True) SR44100 <> BS.pack contents

-- | Generates a standard 128 kb/s mp3 frame with the given sampling rate and
-- arbitrary contents.
genFrameWithSamplingRate :: SamplingRate -> Gen ByteString
genFrameWithSamplingRate sr = do
  let contentsSize' = case sr of
        SR44100 -> 417
        SR48000 -> 384
        SR32000 -> 576
  contents <- vectorOf (contentsSize' - frameHeaderSize) arbitrary
  pure $ mkHeader (PaddingBit False) sr <> BS.pack contents

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
