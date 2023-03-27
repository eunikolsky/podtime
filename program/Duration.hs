module Duration
  ( withCachedDuration
  ) where

import Conduit ((.|), MonadIO, MonadThrow, MonadUnliftIO, liftIO, runConduitRes, sourceFile)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar', readTVarIO)
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.STM (atomically)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M (empty, fromList, insert, lookup, toList)
import Data.Monoid (Any(..))
import Data.Text qualified as T (lines, pack, unlines, unpack)
import Data.Text.IO qualified as T (readFile, writeFile)
import GetDuration (ModTime, MonadDuration(..), MonadDurationCache(..), MonadModTime(..))
import MP3 (AudioDuration, mp3Parser)
import System.Directory (doesFileExist, getModificationTime)

data DurationCache = DurationCache
  -- TODO use text?
  { durationCache :: !(Map (FilePath, ModTime) AudioDuration)
  -- ^ the cache itself
  , anyInserts :: !Any
  -- ^ tracks whether there were any inserts, presumably changing the cache
  }

-- TODO make it a monad transformer?
-- FIXME separate file caching from other two tasks of this type?
-- TODO try lock-based var instead of STM
newtype CachedDurationM a = CachedDurationM (ReaderT (TVar DurationCache) IO a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (TVar DurationCache)
    , MonadIO, MonadUnliftIO, MonadThrow
    )

instance MonadDuration CachedDurationM where
  -- | Returns the audio duration of a single MP3 file. Throws an IO exception
  -- if parsing failed.
  -- TODO return an error instead
  --calculateDuration :: FilePath -> m AudioDuration
  calculateDuration file = runConduitRes $ sourceFile file .| sinkParser mp3Parser

instance MonadDurationCache CachedDurationM where
  getCachedDuration key = do
    cacheVar <- ask
    DurationCache { durationCache } <- liftIO $ readTVarIO cacheVar
    pure $ M.lookup key durationCache

  cacheDuration key duration = do
    cacheVar <- ask
    liftIO . atomically . modifyTVar' cacheVar $ \DurationCache { durationCache, anyInserts } ->
      DurationCache
        { durationCache = M.insert key duration durationCache
        , anyInserts = anyInserts <> Any True
        }

instance MonadModTime CachedDurationM where
  getModTime = liftIO . getModificationTime

-- TODO should it return `IO` or a more generic monad?
withCachedDuration :: CachedDurationM a -> IO a
withCachedDuration (CachedDurationM a) = bracket loadCache saveCache $ runReaderT a

-- important: the return type has to be `TVar Map`, not just `Map` because this
-- value is passed to `saveCache` – if `Map` is used, the same, unmodified map
-- will be saved
loadCache :: IO (TVar DurationCache)
loadCache = do
  exists <- doesFileExist cacheFile
  durationCache <- if exists
    -- TODO support streaming?
    then M.fromList . fmap (read . T.unpack) . T.lines <$> T.readFile cacheFile
    else pure M.empty
  newTVarIO $ DurationCache { durationCache, anyInserts = mempty }

-- | Saves the cache to file only if there were any changes (currently, even if
-- an insert hasn't changed any contents).
saveCache :: TVar DurationCache -> IO ()
saveCache cacheVar = do
  DurationCache { durationCache, anyInserts } <- readTVarIO cacheVar
  when (getAny anyInserts) .
    -- TODO support streaming?
    -- FIXME use a better serialization approach
    T.writeFile cacheFile . T.unlines . fmap (T.pack . show) . M.toList $ durationCache

-- FIXME use the cache location
cacheFile :: FilePath
cacheFile = "duration.cache"
