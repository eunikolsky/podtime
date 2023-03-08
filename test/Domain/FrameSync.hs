module Domain.FrameSync
  ( FrameSync
  , frameSyncBytes
  , mkFrameSync
  , validFrameSync
  ) where

import Data.Bits
import Data.Word

newtype FrameSync = FrameSync Word16

-- | Creates a `FrameSync` from 11 LSBs of the `Word16`.
mkFrameSync :: Word16 -> FrameSync
-- this always clears the 5 LSBs in the result
mkFrameSync = FrameSync . flip shiftL 5

validFrameSync :: FrameSync
validFrameSync = mkFrameSync 0b1111_1111_111

-- | Returns two zeroed frame bytes where only the frame sync bits are set
-- corresponding to `FrameSync`.
frameSyncBytes :: FrameSync -> (Word8, Word8)
frameSyncBytes (FrameSync w16) = (fromIntegral $ w16 `shiftR` 8, fromIntegral w16)

