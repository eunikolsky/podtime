module Domain.MP3HeaderTypes
  ( Bitrate(..)
  , MP3FrameSettings(..)
  , Padding(..)
  , SamplingRate(..)
  , ValidBitrateValue(..)
  , bitrateByte
  , paddingByte
  , samplingRateByte
  ) where

import Data.Bits
import Data.Word

-- | MP3 frame header's settings which define the frame length (in bytes).
data MP3FrameSettings = MP3FrameSettings
  { mfBitrate :: !Bitrate
  , mfSamplingRate :: !SamplingRate
  , mfPadding :: !Padding
  }

data Padding = Padding | NoPadding

instance Show Padding where
  show NoPadding = "padding off"
  show Padding = "padding on"

-- | Returns a zeroed frame byte where only the padding bit is set
-- corresponding to `Padding`.
paddingByte :: Padding -> Word8
paddingByte NoPadding = zeroBits
paddingByte Padding   = 0b00000010

data SamplingRate = SR44100 | SR48000 | SR32000 | SRReserved
  deriving stock (Eq, Ord)

instance Show SamplingRate where
  show SR44100 = "44.1 kHz"
  show SR48000 = "48 kHz"
  show SR32000 = "32 kHz"
  show SRReserved = "<Reserved>"

-- | Returns a zeroed frame byte where only the sampling rate bits are set
-- corresponding to `SamplingRate`.
samplingRateByte :: SamplingRate -> Word8
samplingRateByte SR44100    = zeroBits
samplingRateByte SR48000    = 0b00000100
samplingRateByte SR32000    = 0b00001000
samplingRateByte SRReserved = 0b00001100

-- | Valid bitrate values; they are separate from `Bitrate` in order to generate
-- successful parsing tests for `[minBound..maxBound]`.
data ValidBitrateValue
  = VBV32 | VBV40 | VBV48 | VBV56 | VBV64 | VBV80 | VBV96
  | VBV112 | VBV128 | VBV160 | VBV192 | VBV224 | VBV256 | VBV320
  deriving stock (Bounded, Enum)

data Bitrate = BRValid ValidBitrateValue | BRFree | BRBad

instance Show ValidBitrateValue where
  show VBV32  = "32 kb/s"
  show VBV40  = "40 kb/s"
  show VBV48  = "48 kb/s"
  show VBV56  = "56 kb/s"
  show VBV64  = "64 kb/s"
  show VBV80  = "80 kb/s"
  show VBV96  = "96 kb/s"
  show VBV112 = "112 kb/s"
  show VBV128 = "128 kb/s"
  show VBV160 = "160 kb/s"
  show VBV192 = "192 kb/s"
  show VBV224 = "224 kb/s"
  show VBV256 = "256 kb/s"
  show VBV320 = "320 kb/s"

-- | Returns a zeroed frame byte where only the bitrate bits are set
-- corresponding to `Bitrate`.
bitrateByte :: Bitrate -> Word8
bitrateByte (BRValid VBV32)  = 0b00010000
bitrateByte (BRValid VBV40)  = 0b00100000
bitrateByte (BRValid VBV48)  = 0b00110000
bitrateByte (BRValid VBV56)  = 0b01000000
bitrateByte (BRValid VBV64)  = 0b01010000
bitrateByte (BRValid VBV80)  = 0b01100000
bitrateByte (BRValid VBV96)  = 0b01110000
bitrateByte (BRValid VBV112) = 0b10000000
bitrateByte (BRValid VBV128) = 0b10010000
bitrateByte (BRValid VBV160) = 0b10100000
bitrateByte (BRValid VBV192) = 0b10110000
bitrateByte (BRValid VBV224) = 0b11000000
bitrateByte (BRValid VBV256) = 0b11010000
bitrateByte (BRValid VBV320) = 0b11100000
bitrateByte BRFree           = 0b00000000
bitrateByte BRBad            = 0b11110000
