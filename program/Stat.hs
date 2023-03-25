module Stat
  ( mkStat
  , showStat
  ) where

import Data.Text (Text)
import Lib (formatDuration)
import MP3 (AudioDuration)

-- | The result of running the program, interesting to the user.
newtype Stat = Stat { duration :: AudioDuration }

-- | Renders a `Stat` value as `Text`.
showStat :: Stat -> Text
showStat = formatDuration . duration

-- | Creates a `Stat` value.
mkStat :: AudioDuration -> Stat
mkStat duration = Stat { duration }
