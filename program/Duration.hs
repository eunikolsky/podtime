module Duration
  ( withCachedDuration
  ) where

import Conduit ((.|), MonadIO, MonadThrow, MonadUnliftIO, liftIO, runConduitRes, sourceFile)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar', readTVarIO)
import Control.Exception (bracket)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.STM (atomically)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M (empty, fromList, insert, lookup, toList)
import Data.Text qualified as T (lines, pack, unlines, unpack)
import Data.Text.IO qualified as T (readFile, writeFile)
import GetDuration (ModTime, MonadDuration(..), MonadDurationCache(..), MonadModTime(..))
import MP3 (AudioDuration, mp3Parser)
import System.Directory (doesFileExist, getModificationTime)

-- TODO use text?
type DurationCacheMap = Map (FilePath, ModTime) AudioDuration

-- TODO make it a monad transformer?
-- FIXME separate file caching from other two tasks of this type?
-- TODO try stm-containers
-- FIXME this cache doesn't reduce the running time significantly
newtype CachedDurationM a = CachedDurationM (ReaderT (TVar DurationCacheMap) IO a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (TVar DurationCacheMap)
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
    cache <- liftIO $ readTVarIO cacheVar
    pure $ M.lookup key cache

  cacheDuration key duration = do
    cacheVar <- ask
    liftIO . atomically . modifyTVar' cacheVar $ M.insert key duration

instance MonadModTime CachedDurationM where
  getModTime = liftIO . getModificationTime

-- TODO should it return `IO` or a more generic monad?
withCachedDuration :: CachedDurationM a -> IO a
withCachedDuration (CachedDurationM a) = bracket loadCache saveCache $ runReaderT a

-- important: the return type has to be `TVar Map`, not just `Map` because this
-- value is passed to `saveCache` – if `Map` is used, the same, unmodified map
-- will be saved
loadCache :: IO (TVar DurationCacheMap)
loadCache = do
  exists <- doesFileExist cacheFile
  cache <- if exists
    then M.fromList . fmap (read . T.unpack) . T.lines <$> T.readFile cacheFile
    else pure M.empty
  newTVarIO cache

saveCache :: TVar DurationCacheMap -> IO ()
saveCache cacheVar = do
  cache <- readTVarIO cacheVar
  -- FIXME use a better serialization approach
  T.writeFile cacheFile . T.unlines . fmap (T.pack . show) . M.toList $ cache

-- FIXME use the cache location
cacheFile :: FilePath
cacheFile = "duration.cache"
