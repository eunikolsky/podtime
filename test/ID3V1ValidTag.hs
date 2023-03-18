module ID3V1ValidTag
  ( ValidTag(..)
  , tagSize
  ) where

import Data.ByteString
import Data.ByteString qualified as BS
import Test.QuickCheck

-- | A valid ID3 v1 tag with arbitrary contents.
newtype ValidTag = ValidTag ByteString
  deriving stock (Show)

instance Arbitrary ValidTag where
  arbitrary = do
    contents <- vectorOf (tagSize - 3) arbitrary
    pure . ValidTag $ "TAG" <> BS.pack contents

tagSize :: Int
tagSize = 128
