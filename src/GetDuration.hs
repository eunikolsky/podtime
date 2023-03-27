module GetDuration
  ( MonadDuration(..)
  , MonadDurationCache(..)
  , MonadModTime(..)
  , getDuration
  ) where

import Data.Functor
import Data.Time.Clock
import MP3

-- | Provides a function to calculate duration of an MP3 file.
class Monad m => MonadDuration m where
  calculateDuration :: FilePath -> m AudioDuration

-- | A file modification time.
type ModTime = UTCTime

-- | Interface to access the audio durations cache.
class Monad m => MonadDurationCache m where
  getCachedDuration :: (FilePath, ModTime) -> m (Maybe AudioDuration)
  cacheDuration :: (FilePath, ModTime) -> AudioDuration -> m ()

-- | Provides a function to get the modification time of a file.
class Monad m => MonadModTime m where
  getModTime :: FilePath -> m ModTime

-- | Gets duration of an MP3 file, which may be retrieved from cache.
getDuration :: (MonadDuration m, MonadDurationCache m, MonadModTime m) =>
  FilePath -> m AudioDuration
getDuration fp = do
  modTime <- getModTime fp
  cached <- getCachedDuration (fp, modTime)
  -- note: even though the code may not look like it, but duration is calculated
  -- only if there is no cached duration
  duration <- calculateDuration fp
  maybe
    -- if there is no cached value, cache it and return
    (cacheDuration (fp, modTime) duration $> duration)
    -- else return it w/o caching again
    pure
    cached
