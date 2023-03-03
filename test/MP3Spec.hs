module MP3Spec where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word
import MP3
import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec =
  describe "frameParser" $ do
    it "parses a basic 128 kbps frame" $ do
      let frame = mkFrame
      frameParser `shouldSucceedOn` frame

    it "fails to parse bytes without correct frame sync" $ do
      let frame = mkFrame `replacingHeadWith` 0x00
      frameParser `shouldFailOn` frame

-- | A standard 128 kb/s, 44.1 kHz mp3 frame.
mkFrame :: ByteString
mkFrame = header <> contents
  where header = BS.pack [0xff, 0b11111011, 0b10010000, 0b11000100]
        contents = BS.replicate 417 0

replacingHeadWith :: ByteString -> Word8 -> ByteString
replacingHeadWith bs n = BS.singleton n <> BS.tail bs
