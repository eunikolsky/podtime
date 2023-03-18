module ID3V1Spec (spec) where

import Data.ByteString
import Data.ByteString qualified as BS
import ID3V1
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "id3Parser" $ do
    it "parses ID3 v1 tag" $
      id3Parser `shouldSucceedOn` sampleTag

    prop "fails to parse tag with invalid identifier" $ \(InvalidTag tag) ->
      id3Parser `shouldFailOn` tag

sampleTag :: ByteString
sampleTag = "TAG" <> BS.replicate (tagSize - 3) 0

tagSize :: Int
tagSize = 128

-- | An invalid ID3 v1 tag with 128 arbitrary bytes.
newtype InvalidTag = InvalidTag ByteString
  deriving stock (Show)

instance Arbitrary InvalidTag where
  -- it's unlikely that the bytestring will start with "TAG"
  arbitrary = InvalidTag . BS.pack <$> vectorOf tagSize arbitrary
