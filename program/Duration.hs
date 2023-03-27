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
import Data.Map.Strict qualified as M (empty, insert, lookup)
import GetDuration (ModTime, MonadDuration(..), MonadDurationCache(..), MonadModTime(..))
import MP3 (AudioDuration, mp3Parser)
import System.Directory (getModificationTime)

type DurationCacheMap = Map (FilePath, ModTime) AudioDuration

-- TODO make it a monad transformer?
-- FIXME separate file caching from other two tasks of this type?
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
withCachedDuration (CachedDurationM a) = bracket loadCache saveCache $ \cache -> do
  cacheVar <- newTVarIO cache
  runReaderT a cacheVar

-- FIXME implement
loadCache :: IO DurationCacheMap
loadCache = pure M.empty

-- FIXME implement
saveCache :: DurationCacheMap -> IO ()
saveCache = const $ pure ()
