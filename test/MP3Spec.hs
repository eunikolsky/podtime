module MP3Spec where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import MP3
import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec =
  describe "frameParser" $ do
    it "parses a basic 128 kbps frame" $ do
      let header = "\xff\b11111010\b10010000\b11000100"
          contents = BS.replicate 417 0
          bytes = header <> contents :: ByteString
      frameParser `shouldSucceedOn` bytes
