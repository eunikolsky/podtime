module ID3V1Spec (spec) where

import Data.Attoparsec.ByteString qualified as A
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
    prop "consumes the entire ID3 v1 tag" $ \(ValidTag tag) ->
      -- TODO extract the `complete` combinator
      (id3Parser <* A.endOfInput) `shouldSucceedOn` tag

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
