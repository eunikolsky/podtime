module ID3Spec (spec) where

import AnySizedTag
import Data.Attoparsec.ByteString qualified as A
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word
import Domain.ID3Tag
import ID3
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))
import TestCommon

spec :: Spec
spec = parallel $ do
  describe "id3Parser" $ do
    it "parses a sample ID3 v2.4 tag" $ do
      id3Parser `shouldSucceedOn` sampleID3Tag

    it "parses a sample ID3 v2.3 tag" $ do
      id3Parser `shouldSucceedOn` sampleID3V23Tag

    prop "fails to parse tag with invalid ID3 identifier"
      . forAll genTagWithInvalidID3 $ \tag ->
        id3Parser `shouldFailOn` tag

    prop "fails to parse tag with unsupported version"
      . forAll genTagWithUnsupportedVersion $ \tag ->
        tag ~> id3Parser `shouldFailWithErrorContaining` "Unsupported ID3 version"

    prop "fails to parse tag with any flags"
      . forAll genTagWithFlags $ \tag ->
        tag ~> id3Parser `shouldFailWithErrorContaining` "Unexpected flags"

    -- decrease the number of generated cases per test run because it takes
    -- ~8 seconds to test 100 cases, which is too slow for fast feedback
    modifyMaxSuccess (`div` 10) .
      prop "consumes the entire contents" $ \arbitrarySizedTag ->
        (id3Parser <* A.endOfInput) `shouldSucceedOn` astBytes arbitrarySizedTag

    prop "fails to parse incorrect synchsafe size"
      . forAll genHeaderWithIncorrectSize $ \header ->
        header ~> id3Parser `shouldFailWithErrorContaining` "Incorrect size bytes"

-- | Generates an ID3 v2.4 tag where the identifier "ID3" is replaced with arbitrary bytes.
genTagWithInvalidID3 :: Gen ByteString
genTagWithInvalidID3 = do
  -- it's unlikely that this will generate "ID3"
  identifier <- vectorOf 3 arbitrary
  pure . mkID3Tag $ defaultID3TagSettings { idsIdentifier = BS.pack identifier }

-- | Generates an ID3 tag with an arbitrary, non-2.4 and non-2.3 version.
genTagWithUnsupportedVersion :: Gen ByteString
genTagWithUnsupportedVersion = do
  version <- vectorOf 2 arbitrary
  if version == [4, 0] || version == [3, 0]
    then discard
    else pure . mkID3Tag $ defaultID3TagSettings { idsVersion = BS.pack version }

-- | Generates an ID3 tag with an arbitrary byte for flags.
genTagWithFlags :: Gen ByteString
genTagWithFlags = do
  flags <- getPositive <$> arbitrary
  pure . mkID3Tag $ defaultID3TagSettings { idsFlags = flags }

-- | Generates an ID3 tag header with an arbitrary size where at least one byte
-- has the most-significant bit set.
genHeaderWithIncorrectSize :: Gen ByteString
genHeaderWithIncorrectSize = do
  size :: [Word8] <- vectorOf 4 arbitrary
  byteIndex <- chooseInt (0, 3)
  let replacedSize = (\(index, byte) -> if byteIndex == index then 0b1000_0000 .|. byte else byte)
        <$> zip [0..] size
  pure . mkID3Tag $ defaultID3TagSettings { idsSize = BS.pack replacedSize }
