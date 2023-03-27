module Duration
  ( withCachedDuration
  ) where

import Conduit ((.|), MonadIO, MonadThrow, MonadUnliftIO, liftIO, runConduitRes, sourceFile)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.STM (STM, atomically)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Hashable (Hashable)
import Data.Text qualified as T (lines, pack, unlines, unpack)
import Data.Text.IO qualified as T (readFile, writeFile)
import Data.Time.Clock.Compat ()
import GetDuration (ModTime, MonadDuration(..), MonadDurationCache(..), MonadModTime(..))
import ListT qualified (toList)
import MP3 (AudioDuration, mp3Parser)
import StmContainers.Map qualified as STM (Map)
import StmContainers.Map qualified as STMMap (insert, listT, lookup, new)
import System.Directory (doesFileExist, getModificationTime)

-- TODO use text?
type DurationCacheMap = STM.Map (FilePath, ModTime) AudioDuration

-- TODO make it a monad transformer?
-- FIXME separate file caching from other two tasks of this type?
newtype CachedDurationM a = CachedDurationM (ReaderT DurationCacheMap IO a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader DurationCacheMap
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
    liftIO . atomically $ STMMap.lookup key cacheVar

  cacheDuration key duration = do
    cacheVar <- ask
    liftIO . atomically $ STMMap.insert duration key cacheVar

instance MonadModTime CachedDurationM where
  getModTime = liftIO . getModificationTime

-- TODO should it return `IO` or a more generic monad?
withCachedDuration :: CachedDurationM a -> IO a
withCachedDuration (CachedDurationM a) = bracket loadCache saveCache $ runReaderT a

-- important: the return type has to be `TVar Map`, not just `Map` because this
-- value is passed to `saveCache` – if `Map` is used, the same, unmodified map
-- will be saved
loadCache :: IO DurationCacheMap
loadCache = do
  exists <- doesFileExist cacheFile
  keyValues <- if exists
    then fmap (read . T.unpack) . T.lines <$> T.readFile cacheFile
    else pure mempty
  atomically . stmMapFromList $ keyValues

stmMapFromList :: Hashable k => [(k, v)] -> STM (STM.Map k v)
stmMapFromList keyValues = do
  m <- STMMap.new
  forM_ keyValues $ \(key, value) -> STMMap.insert value key m
  pure m

saveCache :: DurationCacheMap -> IO ()
saveCache cacheVar = do
  -- TODO streaming?
  keyValues <- atomically . ListT.toList $ STMMap.listT cacheVar
  -- FIXME use a better serialization approach
  T.writeFile cacheFile . T.unlines . fmap (T.pack . show) $ keyValues

-- FIXME use the cache location
cacheFile :: FilePath
cacheFile = "duration.cache"
