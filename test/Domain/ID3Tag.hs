module Domain.ID3Tag
  ( ID3TagSettings(..)
  , defaultID3TagSettings
  , mkID3Tag
  , sampleID3Tag
  , sampleID3V23Tag
  , sampleID3V22Tag
  , toSynchsafe
  ) where

import Control.Exception
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor
import Data.Word

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

sampleID3Tag :: ByteString
sampleID3Tag = mkID3Tag defaultID3TagSettings

sampleID3V23Tag :: ByteString
sampleID3V23Tag = mkID3Tag $ defaultID3TagSettings { idsVersion = "\x03\x00" }

sampleID3V22Tag :: ByteString
sampleID3V22Tag = mkID3Tag $ defaultID3TagSettings { idsVersion = "\x02\x00" }

mkID3Tag :: ID3TagSettings -> ByteString
mkID3Tag ids = mconcat [idsIdentifier ids, idsVersion ids, flags, idsSize ids, idsContents ids]
  where
    flags = BS.singleton $ idsFlags ids

-- | Converts the 32-bit (in fact, 28-bit) word into big-endian byte string by
-- putting every 7-bit group into its own byte. For example:
-- `0x0FEE0100` => `[0x7f, 0x38, 0x02, 0x00]`.
toSynchsafe :: Word32 -> ByteString
toSynchsafe x = assert (x < 2 ^ (28 :: Word32)) . BS.pack $
  [3,2..0] <&> (\byteIndex ->
    let bitShift = byteIndex * 7
    in fromIntegral $ 0b0111_1111 .&. x `shiftR` bitShift)
