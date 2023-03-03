module MP3Spec where

import Data.ByteString qualified as BS
import MP3
import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec =
  describe "frameParser" $ do
    it "parses a basic 128 kbps frame" $ do
      let header = BS.pack [0xff, 0b11111011, 0b10010000, 0b11000100]
          contents = BS.replicate 417 0
          bytes = header <> contents
      frameParser `shouldSucceedOn` bytes

    it "fails to parse bytes without correct frame sync" $ do
      let header = BS.pack [0x00, 0b11111011, 0b10010000, 0b11000100]
          contents = BS.replicate 417 0
          bytes = header <> contents
      frameParser `shouldFailOn` bytes
