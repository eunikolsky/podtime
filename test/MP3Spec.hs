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

      it "fails to parse MPEG version 2 frames" $ do
        let header = mkMPEGHeader validFrameSync $ MPEGOther MPEG2 Layer3
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected MPEG version 2 (2) frame"

      it "fails to parse MPEG version 2.5 frames" $ do
        let header = mkMPEGHeader validFrameSync $ MPEGOther MPEG25 Layer3
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected MPEG version 2.5 (0) frame"

      it "fails to parse MPEG version reserved frames" $ do
        let header = mkMPEGHeader validFrameSync $ MPEGOther MPEGReserved Layer3
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected MPEG version \"reserved\" (1) frame"

      -- FIXME combination with mpeg versions
      it "fails to parse MPEG Layer 1 frames" $ do
        let header = mkMPEGHeader validFrameSync $ MPEGOther MPEG1 Layer1
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected Layer 1 (3) frame"

      -- FIXME combination with mpeg versions
      it "fails to parse MPEG Layer 2 frames" $ do
        let header = mkMPEGHeader validFrameSync $ MPEGOther MPEG1 Layer2
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected Layer 2 (2) frame"

      -- FIXME combination with mpeg versions
      it "fails to parse MPEG Layer reserved frames" $ do
        let header = mkMPEGHeader validFrameSync $ MPEGOther MPEG1 LayerReserved
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected Layer \"reserved\" (0) frame"

      it "fails to parse frame with reserved sampling rate" $ do
        let header = mkHeader $ MP3FrameSettings (BRValid VBV128) SRReserved NoPadding
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected sampling rate \"reserved\" (3)"

      it "fails to parse frame with free bitrate" $ do
        let header = mkHeader $ MP3FrameSettings BRFree SR44100 NoPadding
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected bitrate \"free\" (0)"

      it "fails to parse frame with bad bitrate" $ do
        let header = mkHeader $ MP3FrameSettings BRBad SR44100 NoPadding
        header ~> frameParser `shouldFailWithErrorContaining` "Unexpected bitrate \"bad\" (15)"

-- | Checks that parsing result is a failure containing the given string.
--
-- > input ~> parser `shouldFailWithErrorContaining` "foo"
shouldFailWithErrorContaining :: Show a => Either String a -> String -> Expectation
Left err `shouldFailWithErrorContaining` expected = err `shouldContain` expected
Right parsed `shouldFailWithErrorContaining` _ = expectationFailure $ "Unexpectedly parsed " <> show parsed

-- | Parser combinator to make sure the entire input is consumed.
complete :: Parser a -> Parser a
complete = (<* A.endOfInput)

paddingSize :: Padding -> Int
paddingSize NoPadding = 0
paddingSize Padding = 1

data MPEGVersion = MPEG1 | MPEG2 | MPEG25 | MPEGReserved

data Layer = Layer1 | Layer2 | Layer3 | LayerReserved

data MPEGSettings = MP3 !MP3FrameSettings | MPEGOther !MPEGVersion !Layer

mpegVersion :: MPEGSettings -> MPEGVersion
mpegVersion (MP3 _) = MPEG1
mpegVersion (MPEGOther v _) = v

mpegLayer :: MPEGSettings -> Layer
mpegLayer (MP3 _) = Layer3
mpegLayer (MPEGOther _ l) = l

-- | Returns data for an MPEG header with the given settings.
mkMPEGHeader :: FrameSync -> MPEGSettings -> ByteString
-- TODO use Data.Binary.Put ?
mkMPEGHeader frameSync mpeg = BS.pack
  [ byte0
  , byte1
  , byte2
  , 0b11000100
  ]

  where
    (byte0, initialByte1) = frameSyncBytes frameSync
    noProtection = 0b1
    byte1 = orBytes
      [ mpegVersionByte (mpegVersion mpeg)
      , layerByte (mpegLayer mpeg)
      , initialByte1
      , noProtection
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
mkHeader = mkMPEGHeader validFrameSync . MP3

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

-- | A standard 128 kb/s, 44.1 kHz mp3 frame header.
standardMP3Header :: ByteString
standardMP3Header = mkHeader (MP3FrameSettings (BRValid VBV128) SR44100 NoPadding)

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
genFrame :: MP3FrameSettings -> Gen ByteString
genFrame mp3Settings@(MP3FrameSettings br sr padding) = do
  let contentsSize = paddingSize padding + fromMaybe 0 (frameLength sr br)
  contents <- vectorOf (contentsSize - frameHeaderSize `noLessThan` 0) arbitrary
  pure $ mkHeader mp3Settings <> BS.pack contents

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

-- | Generates an mp3 frame header where the frame sync is invalid (11 MSBs are not ones).
genHeaderWithInvalidFrameSync :: Gen ByteString
genHeaderWithInvalidFrameSync = do
  -- `chooseBoundedIntegral` is faster than `choose`
  frameSync <- chooseBoundedIntegral (0, 0b1111_1111_111 - 1)
  pure . mkMPEGHeader (mkFrameSync frameSync) . MP3 $ MP3FrameSettings (BRValid VBV128) SR44100 NoPadding
