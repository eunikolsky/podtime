{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (concat, isSuffixOf)
import Database.SQLite.Simple
import System.Directory (getHomeDirectory)
import System.FilePath.Posix ((</>))
import System.Process (readProcess)

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  allEpisodes <- withConnection (homeDir ++ "/gPodder/Database") $ \conn -> do
    podcasts <- getPodcasts conn
    fmap concat . traverse (getUnheardEpisodes conn) $ podcasts

  print =<< (sumPodcastDurations . fmap ((homeDir ++ "/gPodder/Downloads/") ++)) allEpisodes

{-
 podcasts :: [Int]
 fmap getUnheardEpisodes podcasts :: [IO [String]]
 traverse getUnheardEpisodes podcasts :: IO [[String]]
 fmap join . traverse getUnheardEpisodes $ podcasts :: IO [String]
 -}

-- | Returns a list of all podcasts in gPodder.
getPodcasts :: Connection -> IO [Int]
getPodcasts conn = do
  ids <- query_ conn "SELECT id FROM podcast" :: IO [Only Int]
  return $ fromOnly <$> ids

-- | Returns the filenames of all not-listened-to episodes of the @podcast@ by
-- its id. Only @.mp3@ files are returned.
getUnheardEpisodes :: Connection -> Int -> IO [String]
getUnheardEpisodes conn podcast = do
  r <- queryNamed conn "SELECT p.download_folder, e.download_filename FROM episode e JOIN podcast p ON e.podcast_id = p.id WHERE p.id = :podcast AND e.state = 1 AND e.published >= (SELECT MIN(published) FROM episode WHERE podcast_id = :podcast AND state = 1 AND is_new)" [":podcast" := podcast] :: IO [(String, String)]
  let mp3s = filter (\(_, filename) -> ".mp3" `isSuffixOf` filename) r
  return $ (\(dir, filename) -> dir </> filename) <$> mp3s

-- | Retrieves the durations of the podasts at the @paths@ (using `sox`)
-- and sums them up.
sumPodcastDurations :: [String] -> IO Double
sumPodcastDurations paths = do
  stdout <- readProcess "sox" (["--info", "-D"] ++ paths) ""
  return . sum . fmap read . lines $ stdout
