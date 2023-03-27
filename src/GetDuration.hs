module GetDuration
  ( MonadDuration(..)
  , getDuration
  ) where

import MP3

-- | Provides a function to calculate duration of an MP3 file.
class Monad m => MonadDuration m where
  calculateDuration :: FilePath -> m AudioDuration

-- | Gets duration of an MP3 file, which may be retrieved from cache.
getDuration :: MonadDuration m => FilePath -> m AudioDuration
getDuration = calculateDuration
