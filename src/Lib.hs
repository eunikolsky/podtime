module Lib
  ( formatDuration
  ) where

import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import MP3 (AudioDuration(getAudioDuration))

-- | Converts the floating-point @duration@ to @DiffTime@.
-- The standard function in @Data.Time.Clock@ takes an @Integer@.
durationToDiffTime :: AudioDuration -> DiffTime
durationToDiffTime = picosecondsToDiffTime . floor . (* picosecondsInSecond) . getAudioDuration
  where picosecondsInSecond = 1e12

-- | Formats the audio duration to a more human-readable, simple format.
formatDuration :: AudioDuration -> String
formatDuration = formatTime defaultTimeLocale "%dd %02H:%02M:%02S" . durationToDiffTime
