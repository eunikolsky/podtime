module Duration
  ( withCachedDuration
  ) where

import CacheItemCSV (CacheItemCSV(..), fromKeyValue, toKeyValue)
import Conduit ((.|), MonadIO, MonadThrow, MonadUnliftIO, liftIO, runConduitRes, sourceFile)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar', readTVarIO)
import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.STM (atomically)
import Data.ByteString.Lazy qualified as BL (readFile, writeFile)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Csv (HasHeader(NoHeader), decode, encode)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M (empty, insert, lookup, toList)
import Data.Monoid (Any(..))
import GetDuration (ModTime, MonadDuration(..), MonadDurationCache(..), MonadModTime(..))
import MP3 (AudioDuration, mp3Parser)
import System.Directory (XdgDirectory(XdgCache), createDirectory, doesDirectoryExist, doesFileExist, getModificationTime, getXdgDirectory)
import System.FilePath ((</>))
import System.Posix.Files (ownerModes, setFileMode)
import System.Posix.Types (FileMode)

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

-- | Loads the cache from file. Throws an exception on parsing errors.
--
-- Important: the return type has to be `TVar Map`, not just `Map` because this
-- value is passed to `saveCache` – if `Map` is used, the same, unmodified map
-- will be saved.
loadCache :: IO (TVar DurationCache)
loadCache = do
  cacheFile <- getCacheFilepath
  exists <- doesFileExist cacheFile
  durationCache <- if exists
    then do
      -- TODO support streaming?
      bytes <- BL.readFile cacheFile
      let eitherCache = loadCacheItems <$> decode @CacheItemCSV NoHeader bytes
      handleError eitherCache
    else pure M.empty
  newTVarIO $ DurationCache { durationCache, anyInserts = mempty }

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
saveCache :: TVar DurationCache -> IO ()
saveCache cacheVar = do
  DurationCache { durationCache, anyInserts } <- readTVarIO cacheVar
  when (getAny anyInserts) $ do
    -- TODO support streaming?
    let bytes = encode @CacheItemCSV $ fromKeyValue <$> M.toList durationCache
    cacheFile <- getCacheFilepath
    BL.writeFile cacheFile bytes

-- | Returns the path to the cache file in the program subdirectory in the XDG
-- config directory. Creates the subdirectory if missing.
getCacheFilepath :: IO FilePath
getCacheFilepath = do
  dir <- getXdgDirectory XdgCache "podtime"
  dirExists <- doesDirectoryExist dir
  unless dirExists $
    createDirectory dir >> setFileMode dir userRWX
  pure $ dir </> "duration.cache"

-- | Mode `700` for the created cache subdirectory, as suggested by the
-- XDG Base Directory Specification.
-- https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
userRWX :: FileMode
userRWX = ownerModes
