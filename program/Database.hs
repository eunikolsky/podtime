{-# LANGUAGE QuasiQuotes #-}

module Database
  ( getNewEpisodes
  , getPodcasts
  ) where

import Database.SQLite.Simple
import Text.RawString.QQ (r)

-- | Returns a list of all podcasts in gPodder.
getPodcasts :: Connection -> IO [Int]
getPodcasts conn = do
  ids <- query_ conn "SELECT id FROM podcast" :: IO [Only Int]
  return $ fromOnly <$> ids

-- | Returns the filenames of all not-listened-to episodes of the @podcast@ by
-- its id. Only @.mp3@ files are returned.
getNewEpisodes :: Connection -> Int -> IO [FilePath]
getNewEpisodes conn podcast = do
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
