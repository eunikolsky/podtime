module Lib
  ( formatDuration
  ) where

import AudioDuration (AudioDuration(getAudioDuration))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

-- | Converts the floating-point @duration@ to @DiffTime@.
-- The standard function in @Data.Time.Clock@ takes an @Integer@.
durationToDiffTime :: AudioDuration -> DiffTime
durationToDiffTime = picosecondsToDiffTime . floor . (* picosecondsInSecond) . getAudioDuration
  where picosecondsInSecond = 1e12

-- | Formats the audio duration to a more human-readable, simple format.
formatDuration :: AudioDuration -> Text
formatDuration = T.pack . formatTime defaultTimeLocale "%dd %02H:%02M:%02S" . durationToDiffTime
