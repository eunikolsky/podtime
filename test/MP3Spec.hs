module MP3Spec (spec) where

import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Maybe
import Data.Map.Strict qualified as M
import Domain.FrameSync
import Domain.MP3HeaderTypes
import Domain.MPEGHeaderTypes
import MP3
import Prelude hiding (pred)
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Instances.ByteString ()

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

      prop "fails to parse frames with invalid frame sync"
        . forAll genHeaderWithInvalidFrameSync $ \header ->
          header ~> frameParser `shouldFailWithErrorContaining` "Invalid frame sync"

    describe "examples" $ do
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

      it "fails to parse incomplete frame headers" $ do
        forM_ [1..3] $ \numBytesLeft -> do
          let header = BS.take numBytesLeft standardMP3Header
          header ~> frameParser `shouldFailWithErrorContaining` "Incomplete frame header"

      describe "invalid frame sync error contains hex and ASCII representation" $ do
        it "for ID3 header" $ do
          let id3Header = "ID3\x04\x00" :: ByteString
          id3Header ~> frameParser `shouldFailWithErrorContaining` "Invalid frame sync (0x4944, ID)"

        it "for null header" $ do
          let header = BS.replicate 4 0x00
          header ~> frameParser `shouldFailWithErrorContaining` "Invalid frame sync (0x0000, \NUL\NUL)"

  describe "mp3Parser" $ do
    prop "parses multiple consequent frames" $ \frames ->
      mp3Parser `shouldSucceedOn` validMP3FramesBytes frames

    prop "consumes all (valid) frames" $ \frames ->
      complete mp3Parser `shouldSucceedOn` validMP3FramesBytes frames

    prop "fails on junk before first frame" $ \frames junk ->
      not (BS.null junk) ==>
        -- it's highly unlikely that `junk` will contain a valid MP3 frame
        mp3Parser `shouldFailOn` (junk <> validMP3FramesBytes frames)

    prop "fails on junk after last frame" $ \frames junk ->
      not (BS.null junk) ==>
        -- it's highly unlikely that `junk` will contain a valid MP3 frame
        mp3Parser `shouldFailOn` (validMP3FramesBytes frames <> junk)

    prop "fails on junk between frames" $ \(FramesWithMiddleJunk bytes) ->
      mp3Parser `shouldFailOn` bytes

    forM_ (M.toList frameDurations) $ \(sr, duration) ->
      prop ("calculates the duration of one " <> show sr <> " frame")
        . forAll (genFrame $ MP3FrameSettings (BRValid VBV128) sr NoPadding) $ \frame ->
          frame ~> mp3Parser `parsesDuration` duration

    prop "calculates the duration of all the frames" $ \frames ->
      dfBytes frames ~> mp3Parser `parsesDuration` dfDuration frames

-- | Checks that the parsed duration equals to the expected duration with the
-- precision of `1e-5`.
parsesDuration :: Either String AudioDuration -> AudioDuration -> Expectation
result `parsesDuration` duration = result `parseSatisfies` ((< 1e-5) . abs . (duration -))

newtype ValidMP3Frame = ValidMP3Frame { validMP3FrameBytes :: ByteString }
  deriving newtype (Show)

instance Arbitrary ValidMP3Frame where
  arbitrary = do
    bitrate <- chooseEnum (minBound, maxBound)
    samplingRate <- elements [SR32000, SR44100, SR48000]
    padding <- elements [NoPadding, Padding]
    bytes <- genFrame $ MP3FrameSettings (BRValid bitrate) samplingRate padding
    pure $ ValidMP3Frame bytes

newtype ValidMP3Frames = ValidMP3Frames (NonEmptyList ValidMP3Frame)
  deriving newtype (Arbitrary, Show)

validMP3FramesBytes :: ValidMP3Frames -> ByteString
validMP3FramesBytes (ValidMP3Frames (NonEmpty frames)) = mconcat $ validMP3FrameBytes <$> frames

newtype FramesWithMiddleJunk = FramesWithMiddleJunk ByteString
  deriving newtype (Show)

instance Arbitrary FramesWithMiddleJunk where
  arbitrary = do
    framesBefore <- listOf1 arbitrary
    framesAfter <- listOf1 arbitrary
    junk <- arbitrary
    pure . FramesWithMiddleJunk . mconcat $ concat [framesBefore, [junk], framesAfter]

-- | A wrapper for `MP3FrameSettings` that only prints its `SamplingRate` in `show`
-- (because only that value is relevant to frame duration).
newtype MP3FrameSamplingRateSettings = MP3FrameSamplingRateSettings MP3FrameSettings

instance Show MP3FrameSamplingRateSettings where
  show (MP3FrameSamplingRateSettings s) = show $ mfSamplingRate s

-- | A generated MP3 frame that prints only its settings in `show`.
data MP3Frame = MP3Frame
  { mp3fSettings :: MP3FrameSamplingRateSettings
  , mp3fData :: ByteString
  }

instance Show MP3Frame where
  show = show . mp3fSettings

-- | Arbitrary MP3 frames with their duration.
data DurationFrames = DurationFrames
  { dfFrames :: [MP3Frame]
  -- ^ the type was changed from `ByteString` in order not to print lots of bytes
  -- when test fails, but only the relevant information — sampling rates
  , dfDuration :: AudioDuration
  }
  deriving stock (Show)

dfBytes :: DurationFrames -> ByteString
dfBytes = mconcat . fmap mp3fData . dfFrames

-- | Map from frame's sampling rate to its duration. For MP3, it's calculated
-- as: `1152 / samplingRate`.
frameDurations :: M.Map SamplingRate AudioDuration
frameDurations = M.fromList
  [ (SR44100, 0.026122448)
  , (SR48000, 0.024)
  , (SR32000, 0.036)
  ]

instance Arbitrary DurationFrames where
  arbitrary = do
    sr44100Frames <- listOf1 $ chooseFrame SR44100
    sr48000Frames <- listOf1 $ chooseFrame SR48000
    sr32000Frames <- listOf1 $ chooseFrame SR32000
    let duration = sum
          [ fromIntegral (length sr44100Frames) * frameDurations M.! SR44100
          , fromIntegral (length sr48000Frames) * frameDurations M.! SR48000
          , fromIntegral (length sr32000Frames) * frameDurations M.! SR32000
          ]
    shuffled <- shuffle $ mconcat [sr44100Frames, sr48000Frames, sr32000Frames]
    pure $ DurationFrames shuffled duration

    where
      chooseFrame :: SamplingRate -> Gen MP3Frame
      chooseFrame samplingRate = do
        bitrate <- chooseEnum (minBound, maxBound)
        padding <- elements [NoPadding, Padding]
        let settings = MP3FrameSettings (BRValid bitrate) samplingRate padding
        bytes <- genFrame settings
        pure $ MP3Frame (MP3FrameSamplingRateSettings settings) bytes

-- | Checks that parsing result is a failure containing the given string.
--
-- > input ~> parser `shouldFailWithErrorContaining` "foo"
shouldFailWithErrorContaining :: Show a => Either String a -> String -> Expectation
Left err `shouldFailWithErrorContaining` expected = err `shouldContain` expected
Right parsed `shouldFailWithErrorContaining` _ = expectationFailure $ "Unexpectedly parsed " <> show parsed

-- | Parser combinator to make sure the entire input is consumed.
complete :: Parser a -> Parser a
complete = (<* A.endOfInput)

standardMP3Settings :: MP3FrameSettings
standardMP3Settings = MP3FrameSettings (BRValid VBV128) SR44100 NoPadding

-- | A standard 128 kb/s, 44.1 kHz mp3 frame header.
standardMP3Header :: ByteString
standardMP3Header = mkHeader standardMP3Settings

frameHeaderSize :: Int
frameHeaderSize = 4

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
