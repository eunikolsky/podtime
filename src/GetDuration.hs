module GetDuration
  ( MonadDuration(..)
  , MonadDurationCache(..)
  , getDuration
  ) where

import Data.Maybe
import MP3

-- | Provides a function to calculate duration of an MP3 file.
class Monad m => MonadDuration m where
  calculateDuration :: FilePath -> m AudioDuration

class Monad m => MonadDurationCache m where
  getCachedDuration :: FilePath -> m (Maybe AudioDuration)

-- | Gets duration of an MP3 file, which may be retrieved from cache.
getDuration :: (MonadDuration m, MonadDurationCache m) => FilePath -> m AudioDuration
getDuration fp = do
  cached <- getCachedDuration fp
  duration <- calculateDuration fp
  pure $ fromMaybe duration cached
