module Stat
  ( mkStat
  ) where

import Lib (formatDuration)
import MP3 (AudioDuration)

-- | The result of running the program, interesting to the user.
newtype Stat = Stat { duration :: AudioDuration }

instance Show Stat where
  -- TODO use Text
  show = formatDuration . duration

-- | Creates a `Stat` value.
mkStat :: AudioDuration -> Stat
mkStat duration = Stat { duration }
