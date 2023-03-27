module GetDuration
  ( MonadDuration(..)
  , MonadDurationCache(..)
  , getDuration
  ) where

import Data.Functor
import MP3

-- | Provides a function to calculate duration of an MP3 file.
class Monad m => MonadDuration m where
  calculateDuration :: FilePath -> m AudioDuration

-- | Interface to access the audio durations cache.
class Monad m => MonadDurationCache m where
  getCachedDuration :: FilePath -> m (Maybe AudioDuration)
  cacheDuration :: FilePath -> AudioDuration -> m ()

-- | Gets duration of an MP3 file, which may be retrieved from cache.
getDuration :: (MonadDuration m, MonadDurationCache m) => FilePath -> m AudioDuration
getDuration fp = do
  cached <- getCachedDuration fp
  -- note: even though the code may not look like it, but duration is calculated
  -- only if there is no cached duration
  duration <- calculateDuration fp
  maybe
    -- if there is no cached value, cache it and return
    (cacheDuration fp duration $> duration)
    -- else return it w/o caching again
    pure
    cached
