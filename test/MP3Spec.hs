module MP3Spec (spec) where

import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Maybe
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

    prop "calculates the duration of one 44.1 kHz frame"
      . forAll (genFrame standardMP3Settings) $ \frame ->
        frame ~> mp3Parser `shouldParse` AudioDuration 0.026122

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
validMP3FramesBytes (ValidMP3Frames (NonEmpty frames)) = foldl' BS.append BS.empty $ validMP3FrameBytes <$> frames

newtype FramesWithMiddleJunk = FramesWithMiddleJunk ByteString
  deriving newtype (Show)

instance Arbitrary FramesWithMiddleJunk where
  arbitrary = do
    framesBefore <- listOf1 arbitrary
    framesAfter <- listOf1 arbitrary
    junk <- arbitrary
    pure . FramesWithMiddleJunk $ foldMap' id framesBefore <> junk <> foldMap' id framesAfter

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
