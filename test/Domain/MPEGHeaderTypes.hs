module Domain.MPEGHeaderTypes
  ( Layer(..)
  , MPEGSettings(..)
  , MPEGVersion(..)
  , Protection(..)
  , mkHeader
  , mkMPEGHeader
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Word
import Domain.FrameSync
import Domain.MP3HeaderTypes

data MPEGVersion = MPEG1 | MPEG2 | MPEG25 | MPEGReserved

instance Show MPEGVersion where
  show MPEG1 = "MPEG Version 1"
  show MPEG2 = "MPEG Version 2"
  show MPEG25 = "MPEG Version 2.5"
  show MPEGReserved = "MPEG Version <reserved>"

data Layer = Layer1 | Layer2 | Layer3 | LayerReserved
  deriving stock (Bounded, Enum)

instance Show Layer where
  show Layer1 = "Layer I"
  show Layer2 = "Layer II"
  show Layer3 = "Layer III"
  show LayerReserved = "Layer <reserved>"

data MPEGSettings
  = MP3 !MP3FrameSettings
  | MPEG2Layer3 !MP3FrameSettings
  | MPEGOther !MPEGVersion !Layer

instance Show MPEGSettings where
  show (MP3 _) = "MPEG1 Layer3"
  show (MPEG2Layer3 _) = "MPEG2 Layer3"
  show (MPEGOther v l) = mconcat [show v, ", ", show l]

mpegVersion :: MPEGSettings -> MPEGVersion
mpegVersion (MP3 _) = MPEG1
mpegVersion (MPEG2Layer3 _) = MPEG2
mpegVersion (MPEGOther v _) = v

mpegLayer :: MPEGSettings -> Layer
mpegLayer (MP3 _) = Layer3
mpegLayer (MPEG2Layer3 _) = Layer3
mpegLayer (MPEGOther _ l) = l

data Protection = NotProtected | ProtectedCRC

-- | Returns a zeroed frame byte where only the MPEG version bits are set
-- corresponding to `MPEGVersion`.
mpegVersionByte :: MPEGVersion -> Word8
mpegVersionByte MPEG1        = 0b00011000
mpegVersionByte MPEG2        = 0b00010000
mpegVersionByte MPEG25       = 0b00000000
mpegVersionByte MPEGReserved = 0b00001000

-- | Returns a zeroed frame byte where only the Layer number bits are set
-- corresponding to `Layer`.
layerByte :: Layer -> Word8
layerByte Layer1        = 0b110
layerByte Layer2        = 0b100
layerByte Layer3        = 0b010
layerByte LayerReserved = 0b000

-- | Returns a zeroed frame byte where only the Protection bit is set
-- corresponding to `Protection`.
protectionByte :: Protection -> Word8
protectionByte NotProtected = 1
protectionByte ProtectedCRC = 0

-- | Returns data for MP3 header with the given settings.
mkHeader :: MP3FrameSettings -> ByteString
mkHeader = mkMPEGHeader validFrameSync NotProtected . wrapHeader
  where
    wrapHeader fs@(MP3FrameSettings (MPEG1FrameSettings _ _) _) = MP3 fs
    wrapHeader fs@(MP3FrameSettings (MPEG2FrameSettings _ _) _) = MPEG2Layer3 fs

-- | Returns data for an MPEG header with the given settings.
mkMPEGHeader :: FrameSync -> Protection -> MPEGSettings -> ByteString
-- TODO use Data.Binary.Put ?
mkMPEGHeader frameSync protection mpeg = BS.pack
  [ byte0
  , byte1
  , byte2
  , 0b11000100
  ]

  where
    (byte0, initialByte1) = frameSyncBytes frameSync
    byte1 = orBytes
      [ mpegVersionByte (mpegVersion mpeg)
      , layerByte (mpegLayer mpeg)
      , initialByte1
      , protectionByte protection
      ]
    byte2 = case mpeg of
      MP3 mp3Settings ->
        frameSettingsByte (mfFrameSettings mp3Settings)
          .|. paddingByte (mfPadding mp3Settings)
      MPEG2Layer3 mp3Settings ->
        frameSettingsByte (mfFrameSettings mp3Settings)
          .|. paddingByte (mfPadding mp3Settings)
      _ -> 0

-- | `OR`s all the bytes in the container.
orBytes :: (Bits a, Foldable t) => t a -> a
orBytes = getIor . foldMap' Ior
