{-# LANGUAGE QuasiQuotes #-}

module GPodderDatabase
  ( getNewEpisodes
  , getPodcasts
  , withDatabase
  ) where

import Control.Monad.Reader (ReaderT, ask, liftIO, runReaderT)
import Database.SQLite.Simple (Connection, NamedParam((:=)), Only(..),
                              query_, queryNamed, withConnection)
import Text.RawString.QQ (r)

-- | Internal, opaque type to wrap the database `Connection`.
type DB a = ReaderT Connection IO a

-- | Wraps `withConnection` in order not to require other modules to import
-- `Database.SQLite.Simple`.
withDatabase :: FilePath -> DB a -> IO a
withDatabase file f = withConnection file $ runReaderT f

-- | Returns a list of all podcasts in gPodder.
getPodcasts :: DB [Int]
getPodcasts = do
  conn <- ask
  ids :: [Only Int] <- liftIO $ query_ conn "SELECT id FROM podcast"
  return $ fromOnly <$> ids

-- | Returns the filenames of all not-listened-to episodes of the @podcast@ by
-- its id. Only @.mp3@ and @.m4a@ files are returned. The filenames are relative to
-- gPodder's download directory (they look like `podcast/episode.mp3`).
getNewEpisodes :: Int -> DB [FilePath]
getNewEpisodes podcast = do
  conn <- ask
  results <- liftIO $ queryNamed conn
    [r|
      SELECT p.download_folder || '/' || e.download_filename
      FROM episode e
      JOIN podcast p ON e.podcast_id = p.id
      WHERE p.id = :podcast
        AND e.state = 1
        AND e.published >= (
          SELECT MIN(published) FROM episode WHERE podcast_id = :podcast AND state = 1 AND is_new
        )
        AND (e.download_filename LIKE '%.mp3' OR e.download_filename LIKE '%.m4a')
    |]
    [":podcast" := podcast]
  pure $ fromOnly <$> results
