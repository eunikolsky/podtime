module MP3Spec (spec) where

import AnySizedTag
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Maybe
import Domain.FrameSync
import Domain.ID3Tag
import Domain.MP3HeaderTypes
import Domain.MPEGHeaderTypes
import ID3V1ValidTag qualified as ID3V1
import MP3
import Numeric
import Prelude hiding (pred)
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Instances.ByteString ()
import TestCommon

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

    prop "skips junk before first frame" $ \frame frames (LeadingJunkNoFF junk) ->
      mp3Parser `shouldSucceedOn` (junk <> validMP3FrameBytes frame <> validMP3FramesBytes frames)

    prop "skips leading junk that may contain frame header" $ \frame frames (LeadingJunk junk) ->
      mp3Parser `shouldSucceedOn` (junk <> validMP3FrameBytes frame <> validMP3FramesBytes frames)

    -- this is a specific example based on the now-passing previous property
    -- with seed 1661661415
    it "skips junk containing a valid frame header" $ do
      let frames = mconcat . replicate 2 $ mkFrame standardMP3Settings
          junk = "\x00\x01\x02" <> "\xff\xfb\x90\x00" <> "\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19"
      mp3Parser `shouldSucceedOn` (junk <> frames)

    -- TODO duration with and without junk
    -- TODO fails(?) on longer junk

    prop "fails on junk after last frame" $ \frames junk ->
      not (BS.null junk) && junk /= "\0" ==>
        -- it's highly unlikely that `junk` will contain a valid MP3 frame
        mp3Parser `shouldFailOn` (validMP3FramesBytes frames <> junk)

    prop "fails on junk between frames" $ \(FramesWithMiddleJunk bytes) ->
      mp3Parser `shouldFailOn` bytes

    prop "allows one null byte between frames" $ \(ValidMP3Frame frame0) (ValidMP3Frame frame1) ->
      mp3Parser `shouldSucceedOn` (frame0 <> "\0" <> frame1)

    forM_ (M.toList frameDurations) $ \(sr, duration) ->
      prop ("calculates the duration of one " <> show sr <> " frame")
        . forAll (genFrame $ MP3FrameSettings (BRValid VBV128) sr NoPadding) $ \frame ->
          frame ~> mp3Parser `parsesDuration` duration

    prop "calculates the duration of all the frames" $ \frames ->
      dfBytes frames ~> mp3Parser `parsesDuration` dfDuration frames

    describe "failed end-of-file" $ do
      let junk = " \0\xff\xfb\x50\xc4"

      prop "contains description" $ \(ValidMP3Frame frame) ->
        (frame <> junk) ~> mp3Parser `shouldFailWithErrorContaining` "Expected end-of-file"

      prop "contains position" $ \(ValidMP3Frame frame) -> do
        let position = show $ BS.length frame
        (frame <> junk) ~> mp3Parser `shouldFailWithErrorContaining` position

      prop "contains position in hex" $ \(ValidMP3Frame frame) -> do
        let position = "0x" <> showHex (BS.length frame) ""
        (frame <> junk) ~> mp3Parser `shouldFailWithErrorContaining` position

      prop "contains next 4 bytes" $ \(ValidMP3Frame frame) arbJunk ->
        BS.length arbJunk >= 4 ==> do
          let dump = show . foldMap' BSB.word8HexFixed . BS.unpack $ BS.take 4 arbJunk
        -- this null byte is needed to feed the parser after the last frame so
        -- that it doesn't eat it as part of the after-frame; an alternative
        -- would be to ensure `junk` doesn't start with a null byte
          (frame <> "\0" <> arbJunk) ~> mp3Parser `shouldFailWithErrorContaining` dump

      prop "contains next 1–3 bytes" $ \(ValidMP3Frame frame) (ShortJunk arbJunk) -> do
        let dump = show . foldMap' BSB.word8HexFixed . BS.unpack $ arbJunk
        (frame <> "\0" <> arbJunk) ~> mp3Parser `shouldFailWithErrorContaining` dump

    describe "ID3 support" $ do
      prop "skips ID3 v2 tag before all frames" $ \frames ->
        forAll (resize 12 arbitrary) $ \id3Tag ->
          mp3Parser `shouldSucceedOn` (astBytes id3Tag <> validMP3FramesBytes frames)

      prop "skips post-ID3 null padding bytes" $ \paddingSize ->
        forAll (genFrame $ MP3FrameSettings (BRValid VBV128) SR44100 NoPadding) $ \frame -> do
          let padding = BS.replicate paddingSize 0
          mp3Parser `shouldSucceedOn` (sampleID3V23Tag <> padding <> frame)

      prop "skips post-ID3 space byte"
        . forAll (genFrame $ MP3FrameSettings (BRValid VBV128) SR44100 NoPadding) $ \frame -> do
          let padding = " "
          mp3Parser `shouldSucceedOn` (sampleID3V23Tag <> padding <> frame)

      prop "skips ID3 v1 tag after all frames" $ \frames (ID3V1.ValidTag id3Tag) ->
        mp3Parser `shouldSucceedOn` (validMP3FramesBytes frames <> id3Tag)

      it "does fail with correct message on unsupported ID3 version" $ do
        pendingWith "can't find a way not to backtrack in attoparsec"
        -- I'd expect `mp3Parser` to fail on the ID3 version from the (optional)
        -- inner `id3Parser` because it already parsed "ID3" at the beginning;
        -- however attoparsec backtracks to the beginning and starts parsing the
        -- input as an MP3 frame, which produces a different error message
        -- https://stackoverflow.com/questions/62586114/why-does-attoparsec-need-manytill-if-it-backtracks/62601398#62601398
        let tag = mkID3Tag $ defaultID3TagSettings { idsVersion = "\x05\x00" }
        tag ~> mp3Parser `shouldFailWithErrorContaining` "Unsupported ID3 version"

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
    framesBefore <- validMP3FramesBytes <$> arbitrary
    framesAfter <- validMP3FramesBytes <$> arbitrary
    junk <- BS.pack <$> listOf1 arbitrary
    if junk == "\0"
      then discard
      else pure . FramesWithMiddleJunk . mconcat $ [framesBefore, junk, framesAfter]

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

standardMP3Settings :: MP3FrameSettings
standardMP3Settings = MP3FrameSettings (BRValid VBV128) SR44100 NoPadding

-- | A standard 128 kb/s, 44.1 kHz mp3 frame header.
standardMP3Header :: ByteString
standardMP3Header = mkHeader standardMP3Settings

frameHeaderSize :: Int
frameHeaderSize = 4

mkFrame :: MP3FrameSettings -> ByteString
mkFrame mp3Settings =
  let contentsSize = fromMaybe 0 $ frameLength mp3Settings
  -- TODO how to foolproof myself against forgetting to subtract the frame header size?
  in mkHeader mp3Settings <> BS.replicate (contentsSize - frameHeaderSize `noLessThan` 0) 0

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

-- | 1–3 arbitrary bytes. It's used to test hex dump at expected EOF.
newtype ShortJunk = ShortJunk ByteString
  deriving stock (Show)

instance Arbitrary ShortJunk where
  arbitrary = ShortJunk . BS.pack <$> (flip vectorOf arbitrary =<< chooseInt (1, 3))

-- | Arbitrary junk that is the ending part of a previous frame. It doesn't
-- include a `0xff` byte in order not to confuse the frame parser.
newtype LeadingJunkNoFF = LeadingJunkNoFF ByteString

instance Show LeadingJunkNoFF where
  show (LeadingJunkNoFF bs) = show $ BSB.byteString "LeadingJunkNoFF ("
    <> BSB.intDec (BS.length bs) <> " bytes) "
    <> BSB.byteStringHex bs

instance Arbitrary LeadingJunkNoFF where
  arbitrary = do
    size <- chooseInt (1, 1440 - 1)
    LeadingJunkNoFF . BS.pack <$> vectorOf size (chooseEnum (0, 254))

-- TODO how to deduplicate with `LeadingJunkNoFF`?
-- | Arbitrary junk that is the ending part of a previous frame. It can include
-- a valid frame header bytes.
newtype LeadingJunk = LeadingJunk ByteString

instance Show LeadingJunk where
  show (LeadingJunk bs) = show $ BSB.byteString "LeadingJunk ("
    <> BSB.intDec (BS.length bs) <> " bytes) "
    <> BSB.byteStringHex bs

instance Arbitrary LeadingJunk where
  arbitrary = do
    size <- chooseInt (1, 1440 - 1)
    LeadingJunk . BS.pack <$> vectorOf size arbitrary
