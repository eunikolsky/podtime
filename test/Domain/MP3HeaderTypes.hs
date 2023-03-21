module Domain.MP3HeaderTypes
  ( MP3FrameSettings(..)
  , Padding(..)
  , frameLength
  , paddingByte

  -- * these are reexported from `Domain.MPEG1Types` for now
  , MPEG1.SamplingRate(..)
  , MPEG1.Bitrate(..)
  , MPEG1.ValidBitrateValue(..)
  , MPEG1.bitrateByte
  , MPEG1.samplingRateByte
  ) where

import Data.Bits
import Data.Word
import Domain.MPEG1Types qualified as MPEG1

-- | MP3 frame header's settings which define the frame length (in bytes).
data MP3FrameSettings = MP3FrameSettings
  { mfBitrate :: !MPEG1.Bitrate
  , mfSamplingRate :: !MPEG1.SamplingRate
  , mfPadding :: !Padding
  }

-- | Returns frame length for the `MP3FrameSettings`.
frameLength :: MP3FrameSettings -> Maybe Int
frameLength (MP3FrameSettings br sr padding) =
  (+ paddingSize padding) <$> MPEG1.frameLength br sr

data Padding = Padding | NoPadding

instance Show Padding where
  show NoPadding = "padding off"
  show Padding = "padding on"

paddingSize :: Padding -> Int
paddingSize NoPadding = 0
paddingSize Padding = 1

-- | Returns a zeroed frame byte where only the padding bit is set
-- corresponding to `Padding`.
paddingByte :: Padding -> Word8
paddingByte NoPadding = zeroBits
paddingByte Padding   = 0b00000010
