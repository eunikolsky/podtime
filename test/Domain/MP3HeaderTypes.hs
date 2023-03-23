module Domain.MP3HeaderTypes
  ( FrameSettings(..)
  , MP3FrameSettings(..)
  , Padding(..)
  , frameLength
  , frameSettingsByte
  , paddingByte

  -- * these are reexported from `Domain.MPEG1Types` for now
  , MPEG1.Bitrate(..)
  , MPEG1.SamplingRate(..)
  , MPEG1.ValidBitrateValue(..)
  ) where

import Data.Bits
import Data.Word
import Domain.MPEG1Types qualified as MPEG1
import Domain.MPEG2Types qualified as MPEG2

data FrameSettings
  = MPEG1FrameSettings !MPEG1.Bitrate !MPEG1.SamplingRate
  | MPEG2FrameSettings !MPEG2.Bitrate !MPEG2.SamplingRate

-- | MP3 frame header's settings which define the frame length (in bytes).
data MP3FrameSettings = MP3FrameSettings
  { mfFrameSettings :: !FrameSettings
  , mfPadding :: !Padding
  }

-- | Returns frame length for the `MP3FrameSettings`.
frameLength :: MP3FrameSettings -> Maybe Int
frameLength (MP3FrameSettings frameSettings padding) =
  (+ paddingSize padding) <$> case frameSettings of
    MPEG1FrameSettings br sr -> MPEG1.frameLength br sr
    MPEG2FrameSettings br sr -> MPEG2.frameLength br sr

frameSettingsByte :: FrameSettings -> Word8
frameSettingsByte (MPEG1FrameSettings br sr) = MPEG1.frameSettingsByte br sr
frameSettingsByte (MPEG2FrameSettings br sr) = MPEG2.frameSettingsByte br sr

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
