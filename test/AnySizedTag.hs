module AnySizedTag
  ( AnySizedTag(..)
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Ord
import Data.Word
import Domain.ID3Tag
import Test.QuickCheck
import TestCommon

-- | An ID3 tag with arbitrary contents of arbitrary size. The max
-- size is `2^28 - 1` because it's a synchsafe integer.
data AnySizedTag = AnySizedTag
  { astSize :: Word32
  , astBytes :: ByteString
  }

instance Show AnySizedTag where
  show AnySizedTag { astSize, astBytes } = mconcat
    [ "AnySizedTag ("
    , show astSize
    , " content bytes): "
    , show $ (if shouldTrim then BS.take limit else id) astBytes
    , if shouldTrim then "…" else ""
    ]
    where limit = 16
          shouldTrim = BS.length astBytes > limit

instance Arbitrary AnySizedTag where
  -- | The implementation uses `Gen`'s `size` parameter to limit the maximum
  -- generated tag size — it defines the number of bits used in the arbitrary
  -- tag size, for example `resize 10` will generate sizes up to `2 ^ 10` bytes.
  -- The value is automatically limited to the `[1; 28]` range.
  arbitrary = do
    genSize <- clamp (1, 28) <$> getSize
    size <- chooseEnum (0, (2 ^ genSize) - 1)
    contents <- genContents size
    pure $ AnySizedTag
      { astBytes = mkID3Tag $ defaultID3TagSettings
        { idsSize = toSynchsafe size
        , idsContents = contents
        }
      , astSize = size
      }

    where
      limit = 64

      -- | Generates a bytestring of the given `size`; only first `limit` bytes
      -- are generated arbitrarily, the rest (if any) are zeros. This is because
      -- generating 256 MiB of arbitrary data is pretty slow and unnecessary.
      genContents :: Word32 -> Gen ByteString
      genContents size = do
        let intSize = fromIntegral size
        arbitraryContents <- vectorOf (intSize `noMoreThan` limit) arbitrary
        let nullContents = if intSize > limit then BS.replicate (intSize - limit) 0 else BS.empty
        let contents = BS.pack arbitraryContents <> nullContents
        pure . assert (intSize == BS.length contents) $ contents

  -- I initially tried to regenerate the bytes for every shrunk size, but it's
  -- not possible because that requires the `Gen` monad, but `shrink :: a -> [a]`,
  -- so I'm truncating the previously generated bytes instead, which is more than
  -- enough anyway.
  -- see also: https://stackoverflow.com/questions/59466970/dependent-shrinking-in-quickcheck
  --
  -- idea: this shrinking could be faster by zeroing higher bytes of a given size
  -- before shrinking the size as a number since an error in parsing is more
  -- likely an incorrect (byte by byte) parsing of synchsafe size than using
  -- the size incorrectly
  shrink AnySizedTag { astSize, astBytes } = do
    size <- shrink astSize
    pure $ AnySizedTag
      { astSize = size
      , astBytes = mkID3Tag $ defaultID3TagSettings
        { idsSize = toSynchsafe size
        -- TODO it's inconvenient that this code needs to know the header length
        -- and remember to remove it before taking bytes for the contents
        , idsContents = BS.take (fromIntegral size) . BS.drop id3TagHeaderLength $ astBytes
        }
      }

    where id3TagHeaderLength = 10
