module Lib
  ( formatDuration
  , secondsToDiffTime
  , subgroups
  ) where

import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

-- | Converts the floating-point @duration@ to @DiffTime@.
-- The standard function in @Data.Time.Clock@ takes an @Integer@.
secondsToDiffTime :: Double -> DiffTime
secondsToDiffTime = picosecondsToDiffTime . floor . (* picosecondsInSecond)
  where picosecondsInSecond = 1e12

-- | Formats the duration in seconds to a more human-readable format.
formatDuration :: DiffTime -> String
formatDuration = formatTime defaultTimeLocale "%dd %02H:%02M:%02S"

-- | Splits the list @xs@ into subgroups such that each one contains
-- @maxLen@ items (the last one may contain fewer items).
subgroups :: Int -> [a] -> [[a]]
subgroups _ [] = []
subgroups maxLen xs =
  let (group, rest) = splitAt maxLen xs
  in (group : subgroups maxLen rest)
