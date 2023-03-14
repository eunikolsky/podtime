module ID3Spec (spec) where

import Control.Exception
import Data.Attoparsec.ByteString qualified as A
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor
import Data.Word
import ID3
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))

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

    -- decrease the number of generated cases per test run because it takes
    -- ~8 seconds to test 100 cases, which is too slow for fast feedback
    modifyMaxSuccess (`div` 10) .
      prop "consumes the entire contents" $ \arbitrarySizedTag ->
        (id3Parser <* A.endOfInput) `shouldSucceedOn` sstBytes arbitrarySizedTag

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

-- | An ID3 tag with arbitrary contents of arbitrary size. The max
-- size is `2^28 - 1` because it's a synchsafe integer.
data SmallSizedTag = SmallSizedTag
  { sstSize :: Word32
  , sstBytes :: ByteString
  }

instance Show SmallSizedTag where
  show SmallSizedTag { sstSize, sstBytes } = mconcat
    [ "SmallSizedTag ("
    , show sstSize
    , " content bytes): "
    , show $ (if shouldTrim then BS.take limit else id) sstBytes
    , if shouldTrim then "…" else ""
    ]
    where limit = 16
          shouldTrim = BS.length sstBytes > limit

instance Arbitrary SmallSizedTag where
  arbitrary = do
    size <- chooseEnum (0, (2 ^ (28 :: Word8)) - 1)
    contents <- genContents size
    pure $ SmallSizedTag
      { sstBytes = mkID3Tag $ defaultID3TagSettings
        { idsSize = toSynchsafe size
        , idsContents = contents
        }
      , sstSize = size
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
  shrink SmallSizedTag { sstSize, sstBytes } = do
    size <- shrink sstSize
    pure $ SmallSizedTag
      { sstSize = size
      , sstBytes = mkID3Tag $ defaultID3TagSettings
        { idsSize = toSynchsafe size
        -- TODO it's inconvenient that this code needs to know the header length
        -- and remember to remove it before taking bytes for the contents
        , idsContents = BS.take (fromIntegral size) . BS.drop id3TagHeaderLength $ sstBytes
        }
      }

    where id3TagHeaderLength = 10

-- | Returns the first number if it's <= the second number; otherwise, the second
-- number. It's a more obvious name for `min`.
noMoreThan :: Ord a => a -> a -> a
noMoreThan = min

-- | Converts the 32-bit (in fact, 28-bit) word into big-endian byte string by
-- putting every 7-bit group into its own byte. For example:
-- `0x0FEE0100` => `[0x7f, 0x38, 0x02, 0x00]`.
toSynchsafe :: Word32 -> ByteString
toSynchsafe x = assert (x < 2 ^ (28 :: Word32)) . BS.pack $
  [3,2..0] <&> (\byteIndex ->
    let bitShift = byteIndex * 7
    in fromIntegral $ 0b0111_1111 .&. x `shiftR` bitShift)

data ID3TagSettings = ID3TagSettings
  { idsIdentifier :: ByteString
  , idsVersion :: ByteString
  , idsFlags :: Word8
  , idsSize :: ByteString -- ^ a big-endian synchsafe integer, must be 4 bytes, 7 bits/byte
                          -- (most significant bits of every byte are zeros)
  , idsContents :: ByteString -- ^ `length idsContents` must equal `idsSize`
  }

defaultID3TagSettings :: ID3TagSettings
defaultID3TagSettings = ID3TagSettings "ID3" "\x04\x00" 0 (toSynchsafe 1) "\x00"

mkID3Tag :: ID3TagSettings -> ByteString
mkID3Tag ids = mconcat [idsIdentifier ids, idsVersion ids, flags, idsSize ids, idsContents ids]
  where
    flags = BS.singleton $ idsFlags ids
