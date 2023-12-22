module Domain.MPEG2Types
  ( Bitrate(..)
  , SamplingRate(..)
  , ValidBitrateValue(..)
  , frameLength
  , frameSettingsByte
  ) where

import Data.Bits
import Data.Map.Strict qualified as M
import Data.Word

data SamplingRate = SR22050 | SR24000 | SR16000 | SRReserved
  deriving stock (Eq, Ord)

instance Show SamplingRate where
  show SR22050 = "22.05 kHz"
  show SR24000 = "24 kHz"
  show SR16000 = "16 kHz"
  show SRReserved = "<Reserved>"

-- | Returns a zeroed frame byte where only the sampling rate bits are set
-- corresponding to `SamplingRate`.
samplingRateByte :: SamplingRate -> Word8
samplingRateByte SR22050    = zeroBits
samplingRateByte SR24000    = 0b00000100
samplingRateByte SR16000    = 0b00001000
samplingRateByte SRReserved = 0b00001100

-- | Valid bitrate values; they are separate from `Bitrate` in order to generate
-- successful parsing tests for `[minBound..maxBound]`.
data ValidBitrateValue
  = VBV8 | VBV16 | VBV24 | VBV32 | VBV40 | VBV48 | VBV56
  | VBV64 | VBV80 | VBV96 | VBV112 | VBV128 | VBV144 | VBV160
  deriving stock (Bounded, Enum)

data Bitrate = BRValid ValidBitrateValue | BRFree | BRBad

instance Show ValidBitrateValue where
  show VBV8   = "8 kb/s"
  show VBV16  = "16 kb/s"
  show VBV24  = "24 kb/s"
  show VBV32  = "32 kb/s"
  show VBV40  = "40 kb/s"
  show VBV48  = "48 kb/s"
  show VBV56  = "56 kb/s"
  show VBV64  = "64 kb/s"
  show VBV80  = "80 kb/s"
  show VBV96  = "96 kb/s"
  show VBV112 = "112 kb/s"
  show VBV128 = "128 kb/s"
  show VBV144 = "144 kb/s"
  show VBV160 = "160 kb/s"

-- | Returns a zeroed frame byte where only the bitrate bits are set
-- corresponding to `Bitrate`.
bitrateByte :: Bitrate -> Word8
bitrateByte (BRValid VBV8)   = 0b00010000
bitrateByte (BRValid VBV16)  = 0b00100000
bitrateByte (BRValid VBV24)  = 0b00110000
bitrateByte (BRValid VBV32)  = 0b01000000
bitrateByte (BRValid VBV40)  = 0b01010000
bitrateByte (BRValid VBV48)  = 0b01100000
bitrateByte (BRValid VBV56)  = 0b01110000
bitrateByte (BRValid VBV64)  = 0b10000000
bitrateByte (BRValid VBV80)  = 0b10010000
bitrateByte (BRValid VBV96)  = 0b10100000
bitrateByte (BRValid VBV112) = 0b10110000
bitrateByte (BRValid VBV128) = 0b11000000
bitrateByte (BRValid VBV144) = 0b11010000
bitrateByte (BRValid VBV160) = 0b11100000
bitrateByte BRFree           = 0b00000000
bitrateByte BRBad            = 0b11110000

-- | Returns a zeroed frame byte where only the bitrate and sampling rate bits
-- are set corresponding to `Bitrate` and `SamplingRate`.
frameSettingsByte :: Bitrate -> SamplingRate -> Word8
frameSettingsByte br sr = bitrateByte br .|. samplingRateByte sr

frameLength :: Bitrate -> SamplingRate -> Maybe Int
frameLength _ SRReserved = Nothing
frameLength BRBad _ = Nothing
frameLength BRFree _ = Nothing
frameLength (BRValid vbv) sr = Just $ frameLengths M.! sr !! fromEnum vbv

-- | Map from sampling rate to a list of frame lengths, one for each valid
-- bitrate in the ascending order.
frameLengths :: M.Map SamplingRate [Int]
frameLengths = M.fromList
  -- (\br -> floor $ 576 / 8 * br * 1000 / 22050) <$> [8,16,24,32,40,48,56,64,80,96,112,128,144,160]
  [ (SR22050, [26, 52, 78, 104, 130, 156, 182, 208, 261, 313, 365, 417, 470, 522])
  , (SR24000, [24, 48, 72, 96, 120, 144, 168, 192, 240, 288, 336, 384, 432, 480])
  , (SR16000, [36, 72, 108, 144, 180, 216, 252, 288, 360, 432, 504, 576, 648, 720])
  ]
