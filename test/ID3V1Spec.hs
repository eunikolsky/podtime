module ID3V1Spec (spec) where

import Data.ByteString
import Data.ByteString qualified as BS
import ID3V1
import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = parallel $ do
  describe "id3Parser" $ do
    it "parses ID3 v1 tag" $
      id3Parser `shouldSucceedOn` sampleTag

sampleTag :: ByteString
sampleTag = "TAG" <> BS.replicate (128 - 3) 0
