module ID3Spec (spec) where

import Data.ByteString (ByteString)
import ID3
import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = parallel $ do
  describe "id3Parser" $ do
    it "parses a sample ID3 v2.4 tag" $ do
      id3Parser `shouldSucceedOn` sampleID3Tag

sampleID3Tag :: ByteString
sampleID3Tag = "ID3\x04\x00\x00\x00\x00\x00\x01" <> "\x00"
