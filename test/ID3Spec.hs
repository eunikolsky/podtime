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
sampleID3Tag = mkID3Tag "ID3" "\x04\x00" 0

-- | Generates an ID3 v2.4 tag where the identifier "ID3" is replaced with arbitrary bytes.
genTagWithInvalidID3 :: Gen ByteString
genTagWithInvalidID3 = do
  -- it's unlikely that this will generate "ID3"
  identifier <- vectorOf 3 arbitrary
  pure $ mkID3Tag (BS.pack identifier) "\x04\x00" 0

-- | Generates an ID3 tag with an arbitrary, non-2.4 version.
genTagWithUnsupportedVersion :: Gen ByteString
genTagWithUnsupportedVersion = do
  version <- vectorOf 2 arbitrary
  if version == [4, 0]
    then discard
    else pure $ mkID3Tag "ID3" (BS.pack version) 0

-- | Generates an ID3 tag with an arbitrary byte for flags.
genTagWithFlags :: Gen ByteString
genTagWithFlags = do
  flags <- getPositive <$> arbitrary
  pure $ mkID3Tag "ID3" "\x04\x00" flags

mkID3Tag
  :: ByteString -- ^ identifier
  -> ByteString -- ^ version
  -> Word8 -- ^ flags
  -> ByteString
mkID3Tag identifier version flags = mconcat [identifier, version, BS.singleton flags, size, contents]
  where
    size = "\x00\x00\x00\x01"
    contents = "\x00"
