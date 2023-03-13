module ID3Spec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word
import ID3
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "id3Parser" $ do
    it "parses a sample ID3 v2.4 tag" $ do
      id3Parser `shouldSucceedOn` sampleID3Tag

    prop "fails to parse tag with invalid ID3 identifier"
      . forAll genTagWithInvalidID3 $ \tag ->
        id3Parser `shouldFailOn` tag

    prop "fails to parse non-v2.4 tag"
      . forAll genTagWithUnsupportedVersion $ \tag ->
        id3Parser `shouldFailOn` tag

    prop "fails to parse tag with any flags"
      . forAll genTagWithFlags $ \tag ->
        id3Parser `shouldFailOn` tag

sampleID3Tag :: ByteString
sampleID3Tag = mkID3Tag defaultID3TagSettings

-- | Generates an ID3 v2.4 tag where the identifier "ID3" is replaced with arbitrary bytes.
genTagWithInvalidID3 :: Gen ByteString
genTagWithInvalidID3 = do
  -- it's unlikely that this will generate "ID3"
  identifier <- vectorOf 3 arbitrary
  pure . mkID3Tag $ defaultID3TagSettings { idsIdentifier = BS.pack identifier }

-- | Generates an ID3 tag with an arbitrary, non-2.4 version.
genTagWithUnsupportedVersion :: Gen ByteString
genTagWithUnsupportedVersion = do
  version <- vectorOf 2 arbitrary
  if version == [4, 0]
    then discard
    else pure . mkID3Tag $ defaultID3TagSettings { idsVersion = BS.pack version }

-- | Generates an ID3 tag with an arbitrary byte for flags.
genTagWithFlags :: Gen ByteString
genTagWithFlags = do
  flags <- getPositive <$> arbitrary
  pure . mkID3Tag $ defaultID3TagSettings { idsFlags = flags }

data ID3TagSettings = ID3TagSettings
  { idsIdentifier :: ByteString
  , idsVersion :: ByteString
  , idsFlags :: Word8
  }

defaultID3TagSettings :: ID3TagSettings
defaultID3TagSettings = ID3TagSettings "ID3" "\x04\x00" 0

mkID3Tag :: ID3TagSettings -> ByteString
mkID3Tag ids = mconcat [idsIdentifier ids, idsVersion ids, flags, size, contents]
  where
    flags = BS.singleton $ idsFlags ids
    size = "\x00\x00\x00\x01"
    contents = "\x00"
