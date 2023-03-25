{-# LANGUAGE QuasiQuotes #-}

module Database
  ( getNewEpisodes
  , getPodcasts
  , withDatabase
  ) where

import Database.SQLite.Simple (Connection, NamedParam((:=)), Only(..),
                              query_, queryNamed, withConnection)
import Text.RawString.QQ (r)

-- | Internal, opaque type to wrap the database `Connection`.
newtype DB = DB Connection

-- | Wraps `withConnection` in order not to require other modules to import
-- `Database.SQLite.Simple`.
withDatabase :: FilePath -> (DB -> IO a) -> IO a
withDatabase file f = withConnection file $ f . DB

-- | Returns a list of all podcasts in gPodder.
getPodcasts :: DB -> IO [Int]
getPodcasts (DB conn) = do
  ids <- query_ conn "SELECT id FROM podcast" :: IO [Only Int]
  return $ fromOnly <$> ids

-- | Returns the filenames of all not-listened-to episodes of the @podcast@ by
-- its id. Only @.mp3@ files are returned.
getNewEpisodes :: DB -> Int -> IO [FilePath]
getNewEpisodes (DB conn) podcast = do
  results <- queryNamed conn
    [r|
      SELECT p.download_folder || '/' || e.download_filename
      FROM episode e
      JOIN podcast p ON e.podcast_id = p.id
      WHERE p.id = :podcast
        AND e.state = 1
        AND e.published >= (
          SELECT MIN(published) FROM episode WHERE podcast_id = :podcast AND state = 1 AND is_new
        )
        AND e.download_filename LIKE '%.mp3'
    |]
    [":podcast" := podcast]
  pure $ fromOnly <$> results
