module MP3Spec (spec) where

import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Maybe
import Data.Word
import Domain.FrameSync
import Domain.MP3HeaderTypes
import MP3
import Prelude hiding (pred)
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))

spec :: Spec
spec = parallel $ do
  describe "frameParser" $ do
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
              forAll (genFrame $ MP3FrameSettings (BRValid bitrate) samplingRate padding) $ \frame ->
                complete frameParser `shouldSucceedOn` frame

      prop "fails to parse bytes with invalid frame sync"
        . forAll genHeaderWithInvalidFrameSync $ \header ->
          header ~> frameParser `shouldFailWithErrorContaining` "Invalid frame sync"

    describe "examples" $ do
      it "parses a basic 128 kbps frame" $ do
        let frame = mkFrame
        complete frameParser `shouldSucceedOn` frame

      forM_ [ (MPEG2, "2 (2)")
            , (MPEG25, "2.5 (0)")
            , (MPEGReserved, "\"reserved\" (1)")
            ] $ \(version, versionDesc) ->
        forM_ [minBound..maxBound] $ \layer -> do
          let settings = MPEGOther version layer
          it ("fails to parse " <> show settings <> " frames") $ do
            let header = mkMPEGHeader validFrameSync NotProtected settings
            header ~> frameParser `shouldFailWithErrorContaining`
              ("Unexpected MPEG version " <> versionDesc <> " frame")

      forM_ [ (Layer1, "1 (3)")
            , (Layer2, "2 (2)")
            , (LayerReserved, "\"reserved\" (0)")
            ] $ \(layer, layerDesc) -> do
        let settings = MPEGOther MPEG1 layer
        it ("fails to parse " <> show settings <> " frames") $ do
          let header = mkMPEGHeader validFrameSync NotProtected settings
          header ~> frameParser `shouldFailWithErrorContaining`
            ("Unexpected Layer " <> layerDesc <> " frame")

      it "fails to parse frame with reserved sampling rate" $ do
        let header = mkHeader $ MP3FrameSettings (BRValid VBV128) SRReserved NoPadding
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected sampling rate \"reserved\" (3)"

      it "fails to parse frame with free bitrate" $ do
        let header = mkHeader $ MP3FrameSettings BRFree SR44100 NoPadding
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected bitrate \"free\" (0)"

      it "fails to parse frame with bad bitrate" $ do
        let header = mkHeader $ MP3FrameSettings BRBad SR44100 NoPadding
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected bitrate \"bad\" (15)"

      it "fails to parse frame with protection" $ do
        let header = mkMPEGHeader validFrameSync ProtectedCRC $ MP3 standardMP3Settings
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected CRC-protected (0) frame"

-- | Checks that parsing result is a failure containing the given string.
--
-- > input ~> parser `shouldFailWithErrorContaining` "foo"
shouldFailWithErrorContaining :: Show a => Either String a -> String -> Expectation
Left err `shouldFailWithErrorContaining` expected = err `shouldContain` expected
Right parsed `shouldFailWithErrorContaining` _ = expectationFailure $ "Unexpectedly parsed " <> show parsed

-- | Parser combinator to make sure the entire input is consumed.
complete :: Parser a -> Parser a
complete = (<* A.endOfInput)

data MPEGVersion = MPEG1 | MPEG2 | MPEG25 | MPEGReserved

instance Show MPEGVersion where
  show MPEG1 = "MPEG Version 1"
  show MPEG2 = "MPEG Version 2"
  show MPEG25 = "MPEG Version 2.5"
  show MPEGReserved = "MPEG Version <reserved>"

data Layer = Layer1 | Layer2 | Layer3 | LayerReserved
  deriving stock (Bounded, Enum)

instance Show Layer where
  show Layer1 = "Layer I"
  show Layer2 = "Layer II"
  show Layer3 = "Layer III"
  show LayerReserved = "Layer <reserved>"

data MPEGSettings = MP3 !MP3FrameSettings | MPEGOther !MPEGVersion !Layer

instance Show MPEGSettings where
  show (MP3 _) = "MP3"
  show (MPEGOther v l) = mconcat [show v, ", ", show l]

mpegVersion :: MPEGSettings -> MPEGVersion
mpegVersion (MP3 _) = MPEG1
mpegVersion (MPEGOther v _) = v

mpegLayer :: MPEGSettings -> Layer
mpegLayer (MP3 _) = Layer3
mpegLayer (MPEGOther _ l) = l

data Protection = NotProtected | ProtectedCRC

-- | Returns data for an MPEG header with the given settings.
mkMPEGHeader :: FrameSync -> Protection -> MPEGSettings -> ByteString
-- TODO use Data.Binary.Put ?
mkMPEGHeader frameSync protection mpeg = BS.pack
  [ byte0
  , byte1
  , byte2
  , 0b11000100
  ]

  where
    (byte0, initialByte1) = frameSyncBytes frameSync
    byte1 = orBytes
      [ mpegVersionByte (mpegVersion mpeg)
      , layerByte (mpegLayer mpeg)
      , initialByte1
      , protectionByte protection
      ]
    byte2 = case mpeg of
      MP3 mp3Settings -> orBytes
        [ bitrateByte $ mfBitrate mp3Settings
        , samplingRateByte $ mfSamplingRate mp3Settings
        , paddingByte $ mfPadding mp3Settings
        ]
      _ -> 0

-- | `OR`s all the bytes in the container.
orBytes :: (Bits a, Foldable t) => t a -> a
orBytes = getIor . foldMap' Ior

-- | Returns data for MP3 header with the given settings.
mkHeader :: MP3FrameSettings -> ByteString
mkHeader = mkMPEGHeader validFrameSync NotProtected . MP3

-- | Returns a zeroed frame byte where only the MPEG version bits are set
-- corresponding to `MPEGVersion`.
mpegVersionByte :: MPEGVersion -> Word8
mpegVersionByte MPEG1        = 0b00011000
mpegVersionByte MPEG2        = 0b00010000
mpegVersionByte MPEG25       = 0b00000000
mpegVersionByte MPEGReserved = 0b00001000

-- | Returns a zeroed frame byte where only the Layer number bits are set
-- corresponding to `Layer`.
layerByte :: Layer -> Word8
layerByte Layer1        = 0b110
layerByte Layer2        = 0b100
layerByte Layer3        = 0b010
layerByte LayerReserved = 0b000

-- | Returns a zeroed frame byte where only the Protection bit is set
-- corresponding to `Protection`.
protectionByte :: Protection -> Word8
protectionByte NotProtected = 1
protectionByte ProtectedCRC = 0

standardMP3Settings :: MP3FrameSettings
standardMP3Settings = MP3FrameSettings (BRValid VBV128) SR44100 NoPadding

-- | A standard 128 kb/s, 44.1 kHz mp3 frame header.
standardMP3Header :: ByteString
standardMP3Header = mkHeader standardMP3Settings

frameHeaderSize :: Int
frameHeaderSize = 4

-- | A standard 128 kb/s, 44.1 kHz mp3 frame.
mkFrame :: ByteString
mkFrame = standardMP3Header <> contents
  where
    contents = BS.replicate contentsSize 0
    contentsSize = fromJust (frameLength standardMP3Settings) - frameHeaderSize

-- | Generates an mp3 frame with the given sampling rate, bitrate and padding,
-- and arbitrary contents.
genFrame :: MP3FrameSettings -> Gen ByteString
genFrame mp3Settings = do
  let contentsSize = fromMaybe 0 $ frameLength mp3Settings
  contents <- vectorOf (contentsSize - frameHeaderSize `noLessThan` 0) arbitrary
  pure $ mkHeader mp3Settings <> BS.pack contents

-- | Returns the first number if it's >= the second number; otherwise, the second
-- number. It's a more obvious name for `max`.
noLessThan :: Ord a => a -> a -> a
noLessThan = max

-- | Generates an mp3 frame header where the frame sync is invalid (11 MSBs are not ones).
genHeaderWithInvalidFrameSync :: Gen ByteString
genHeaderWithInvalidFrameSync = do
  -- `chooseBoundedIntegral` is faster than `choose`
  frameSync <- chooseBoundedIntegral (0, 0b1111_1111_111 - 1)
  pure . mkMPEGHeader (mkFrameSync frameSync) NotProtected . MP3 $ standardMP3Settings
