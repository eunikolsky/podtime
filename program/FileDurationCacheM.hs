module FileDurationCacheM
  ( withFileDurationCache
  ) where

import CacheItemCSV (CacheItemCSV(..), fromKeyValue, toKeyValue)
import Conduit (MonadIO, MonadThrow, MonadTrans, MonadUnliftIO, lift, liftIO)
import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.ByteString.Lazy qualified as BL (readFile, writeFile)
import Data.Csv (HasHeader(NoHeader), decode, encode)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M (empty, insert, lookup, toList)
import Data.Monoid (Any(..))
import GetDuration (ModTime, MonadDuration(..), MonadDurationCache(..), MonadModTime(..))
import MP3 (AudioDuration)
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Exception (bracket, throwIO)
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import XDGDir (XDGDir(XDGCache), getXDGPath)

data DurationCache = DurationCache
  -- TODO use text?
  { durationCache :: !(Map (FilePath, ModTime) AudioDuration)
  -- ^ the cache itself
  , anyInserts :: !Any
  -- ^ tracks whether there were any inserts, presumably changing the cache
  }

-- | Provides file-based duration cache.
newtype FileDurationCacheM m a = FileDurationCacheM (ReaderT (IORef DurationCache) m a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (IORef DurationCache)
    , MonadTrans, MonadIO, MonadUnliftIO, MonadThrow
    )

withFileDurationCache :: MonadUnliftIO m => FileDurationCacheM m a -> m a
withFileDurationCache (FileDurationCacheM a) = bracket loadCache saveCache $ runReaderT a

instance MonadIO m => MonadDurationCache (FileDurationCacheM m) where
  getCachedDuration key = do
    cacheVar <- ask
    DurationCache { durationCache } <- liftIO $ readIORef cacheVar
    pure $ M.lookup key durationCache

  cacheDuration key duration = do
    cacheVar <- ask
    liftIO . atomicModifyIORef' cacheVar $ \DurationCache { durationCache, anyInserts } ->
      ( DurationCache
          { durationCache = M.insert key duration durationCache
          , anyInserts = anyInserts <> Any True
          }
        , ()
      )

instance (MonadModTime m, MonadIO m) => MonadModTime (FileDurationCacheM m) where
  getModTime = liftIO . getModTime

instance MonadDuration m => MonadDuration (FileDurationCacheM m) where
  calculateDuration = lift . calculateDuration

-- | Loads the cache from file. Throws an exception on parsing errors.
--
-- Important: the return type has to be `IORef Map`, not just `Map` because this
-- value is passed to `saveCache` – if `Map` is used, the same, unmodified map
-- will be saved.
loadCache :: MonadIO m => m (IORef DurationCache)
loadCache = do
  cacheFile <- getCacheFilepath
  exists <- doesFileExist cacheFile
  durationCache <- if exists
    then do
      -- TODO support streaming?
      bytes <- liftIO $ BL.readFile cacheFile
      let eitherCache = loadCacheItems <$> decode @CacheItemCSV NoHeader bytes
      handleError eitherCache
    else pure M.empty
  newIORef $ DurationCache { durationCache, anyInserts = mempty }

  where handleError = either (throwIO . LoadCacheException) pure

newtype LoadCacheException = LoadCacheException String
  deriving stock Show

instance Exception LoadCacheException

loadCacheItems :: Foldable f => f CacheItemCSV -> Map (FilePath, ModTime) AudioDuration
loadCacheItems = foldl'
  (\m item -> let ((fp, mtime), dur) = toKeyValue item in M.insert (fp, mtime) dur m)
  M.empty

-- | Saves the cache to file only if there were any changes (currently, even if
-- an insert hasn't changed any contents).
saveCache :: MonadIO m => IORef DurationCache -> m ()
saveCache cacheVar = do
  DurationCache { durationCache, anyInserts } <- readIORef cacheVar
  when (getAny anyInserts) $ do
    -- TODO support streaming?
    let bytes = encode @CacheItemCSV $ fromKeyValue <$> M.toList durationCache
    cacheFile <- getCacheFilepath
    liftIO $ BL.writeFile cacheFile bytes

-- | Returns the path to the cache file in the program subdirectory in the XDG
-- config directory. Creates the subdirectory if missing.
getCacheFilepath :: MonadIO m => m FilePath
getCacheFilepath = getXDGPath XDGCache "duration.cache"
