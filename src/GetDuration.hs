module GetDuration
  ( MonadDuration(..)
  , MonadDurationCache(..)
  , MonadModTime(..)
  , ModTime
  , getDuration
  ) where

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
  -- note: this `calculated` value is a description of how to calculate a
  -- duration, it's executed only when there is no cached value
  let calculated = do
        duration <- calculateDuration fp
        cacheDuration (fp, modTime) duration
        pure duration
  maybe calculated pure cached
