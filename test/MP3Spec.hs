module MP3Spec (spec) where

import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Maybe
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

      it "fails to parse MPEG version 2.5 frames" $ do
        let header = mkMPEGHeader MPEG25 NoPadding SR44100 (BRValid VBV128)
        frameParser `shouldFailOn` header

    describe "properties" $ do
      forM_ [NoPadding, Padding] $ \padding ->
        forM_ [SR44100, SR48000, SR32000] $ \samplingRate ->
          forM_ [minBound..maxBound] $ \bitrate -> do
            let desciption = mconcat
                  [ "parses a "
                  , show bitrate, ", "
                  , show samplingRate, ", "
                  , show padding
                  , " frame"
                  ]
            modifyMaxSuccess (`div` 10) . prop desciption .
              forAll (genFrame samplingRate (BRValid bitrate) padding) $ \frame ->
                complete frameParser `shouldSucceedOn` frame

      prop "fails to parse bytes with incorrect first byte"
        . forAll genInvalidFrame $ \frame ->
          frameParser `shouldFailOn` frame

      prop "fails to parse frame with reserved sampling rate"
        . forAll (genFrame SRReserved (BRValid VBV128) NoPadding) $ \frame ->
          case frame ~> frameParser of
            Left err -> err `shouldContain` "Unexpected sampling rate \"reserved\" (3)"
            Right parsed -> expectationFailure $ "parsed frame " <> show parsed

      prop "fails to parse frame with free bitrate"
        . forAll (genFrame SR44100 BRFree NoPadding) $ \frame ->
          case frame ~> frameParser of
            Left err -> err `shouldContain` "Unexpected bitrate \"free\" (0)"
            Right parsed -> expectationFailure $ "parsed frame " <> show parsed

      prop "fails to parse frame with bad bitrate"
        . forAll (genFrame SR44100 BRBad NoPadding) $ \frame ->
          case frame ~> frameParser of
            Left err -> err `shouldContain` "Unexpected bitrate \"bad\" (15)"
            Right parsed -> expectationFailure $ "parsed frame " <> show parsed

-- | Parser combinator to make sure the entire input is consumed.
complete :: Parser a -> Parser a
complete = (<* A.endOfInput)

data Padding = Padding | NoPadding

instance Show Padding where
  show NoPadding = "padding off"
  show Padding = "padding on"

paddingSize :: Padding -> Int
paddingSize NoPadding = 0
paddingSize Padding = 1

data SamplingRate = SR44100 | SR48000 | SR32000 | SRReserved
  deriving stock (Eq, Ord)

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

data MPEGVersion = MPEG1 | MPEG25

-- | Returns data for an MPEG header with the given settings.
mkMPEGHeader :: MPEGVersion -> Padding -> SamplingRate -> Bitrate -> ByteString
mkMPEGHeader mpeg padding sr br = BS.pack
  [ 0xff
  , byte1
  , byte2
  , 0b11000100
  ]

  where
    byte1 = mpegVersionByte mpeg .|. 0b11100011
    byte2 = getIor $ foldMap' Ior
      [ bitrateByte br
      , samplingRateByte sr
      , paddingByte padding
      ]

-- | Returns data for MP3 header with the given settings.
mkHeader :: Padding -> SamplingRate -> Bitrate -> ByteString
mkHeader = mkMPEGHeader MPEG1

-- | Returns a zeroed frame byte where only the MPEG version bits are set
-- corresponding to `MPEGVersion`.
mpegVersionByte :: MPEGVersion -> Word8
mpegVersionByte MPEG1  = 0b00011000
mpegVersionByte MPEG25 = 0b00000000

-- | Returns a zeroed frame byte where only the padding bit is set
-- corresponding to `Padding`.
paddingByte :: Padding -> Word8
paddingByte NoPadding = zeroBits
paddingByte Padding   = 0b00000010

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
bitrateByte (BRValid VBV32)  = 0b00010000
bitrateByte (BRValid VBV40)  = 0b00100000
bitrateByte (BRValid VBV48)  = 0b00110000
bitrateByte (BRValid VBV56)  = 0b01000000
bitrateByte (BRValid VBV64)  = 0b01010000
bitrateByte (BRValid VBV80)  = 0b01100000
bitrateByte (BRValid VBV96)  = 0b01110000
bitrateByte (BRValid VBV112) = 0b10000000
bitrateByte (BRValid VBV128) = 0b10010000
bitrateByte (BRValid VBV160) = 0b10100000
bitrateByte (BRValid VBV192) = 0b10110000
bitrateByte (BRValid VBV224) = 0b11000000
bitrateByte (BRValid VBV256) = 0b11010000
bitrateByte (BRValid VBV320) = 0b11100000
bitrateByte BRFree           = 0b00000000
bitrateByte BRBad            = 0b11110000

-- | A standard 128 kb/s, 44.1 kHz mp3 frame header.
standardMP3Header :: ByteString
standardMP3Header = mkHeader NoPadding SR44100 (BRValid VBV128)

frameHeaderSize :: Int
frameHeaderSize = 4

-- | A standard 128 kb/s, 44.1 kHz mp3 frame.
mkFrame :: ByteString
mkFrame = standardMP3Header <> contents
  where
    contents = BS.replicate contentsSize 0
    contentsSize = fromJust (frameLength SR44100 $ BRValid VBV128) - frameHeaderSize

-- | Generates an mp3 frame with the given sampling rate, bitrate and padding,
-- and arbitrary contents.
genFrame :: SamplingRate -> Bitrate -> Padding -> Gen ByteString
genFrame sr br padding = do
  let contentsSize = paddingSize padding + fromMaybe 0 (frameLength sr br)
  contents <- vectorOf (contentsSize - frameHeaderSize `noLessThan` 0) arbitrary
  pure $ mkHeader padding sr br <> BS.pack contents

-- | Returns frame length for the sampling rate and bitrate.
frameLength :: SamplingRate -> Bitrate -> Maybe Int
frameLength SRReserved _ = Nothing
frameLength _ BRBad = Nothing
frameLength _ BRFree = Nothing
frameLength sr (BRValid vbv) = Just $ frameLengths M.! sr !! fromEnum vbv

-- | Map from sampling rate to a list of frame lengths, one for each valid
-- bitrate in the ascending order.
frameLengths :: M.Map SamplingRate [Int]
frameLengths = M.fromList
  [ (SR32000, [144, 180, 216, 252, 288, 360, 432, 504, 576, 720, 864, 1008, 1152, 1440])
  , (SR44100, [104, 130, 156, 182, 208, 261, 313, 365, 417, 522, 626, 731, 835, 1044])
  , (SR48000, [96, 120, 144, 168, 192, 240, 288, 336, 384, 480, 576, 672, 768, 960])
  ]

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
  frame <- genFrame SR44100 (BRValid VBV128) NoPadding
  pure $ frame `replacingHeadWith` firstByte

firstM :: (Monad m, Show a) => (a -> Bool) -> [m a] -> m a
firstM pred (x:xs) = do
  x' <- x
  if pred x' then pure x' else firstM pred xs
firstM _ [] = error "Unexpected empty xs in firstM"

replacingHeadWith :: ByteString -> Word8 -> ByteString
replacingHeadWith bs n = BS.singleton n <> BS.tail bs
