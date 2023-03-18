module ID3V1Spec (spec) where

import Data.ByteString
import Data.ByteString qualified as BS
import ID3V1
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import TestCommon

spec :: Spec
spec = parallel $ do
  describe "id3Parser" $ do
    prop "consumes the entire ID3 v1 tag" $ \(ValidTag tag) ->
      complete id3Parser `shouldSucceedOn` tag

    prop "consumes only the tag contents" $ \(ValidTag tag) bytes ->
      not (BS.null bytes) ==>
        (tag <> bytes) ~?> id3Parser `leavesUnconsumed` bytes

    prop "fails to parse tag with invalid identifier" $ \(InvalidTag tag) ->
      id3Parser `shouldFailOn` tag

tagSize :: Int
tagSize = 128

-- | A valid ID3 v1 tag with arbitrary contents.
newtype ValidTag = ValidTag ByteString
  deriving stock (Show)

instance Arbitrary ValidTag where
  arbitrary = do
    contents <- vectorOf (tagSize - 3) arbitrary
    pure . ValidTag $ "TAG" <> BS.pack contents

-- | An invalid ID3 v1 tag with 128 arbitrary bytes.
newtype InvalidTag = InvalidTag ByteString
  deriving stock (Show)

instance Arbitrary InvalidTag where
  -- it's unlikely that the bytestring will start with "TAG"
  arbitrary = InvalidTag . BS.pack <$> vectorOf tagSize arbitrary
