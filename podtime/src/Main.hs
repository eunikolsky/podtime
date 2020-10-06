{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (concat, isSuffixOf)
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.SQLite.Simple
import System.Directory (getHomeDirectory)
import System.FilePath.Posix ((</>))
import System.IO (IOMode(..), withBinaryFile)
import System.Process (StdStream(..), cwd, proc, readCreateProcess, std_err)

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let gPodderHome = homeDir </> "gPodder"
  allEpisodes <- withConnection (gPodderHome </> "Database") $ \conn -> do
    podcasts <- getPodcasts conn
    fmap concat . traverse (getUnheardEpisodes conn) $ podcasts

  duration <- sumPodcastDurations (gPodderHome </> "Downloads") allEpisodes
  putStrLn . formatDuration . secondsToDiffTime $ duration

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
-- and sums them up. The @paths@ should be relative to @gPodderDownloads@.
sumPodcastDurations :: FilePath -> [String] -> IO Double
sumPodcastDurations gPodderDownloads paths =
  withBinaryFile "/dev/null" WriteMode $ \dev_null -> do
    stdout <- readCreateProcess (sox gPodderDownloads dev_null) ""
    return . sum . fmap read . lines $ stdout

  where
    sox gPodderDownloads dev_null = (proc "sox" (["--info", "-D"] ++ paths))
      { cwd = Just gPodderDownloads
      , std_err = UseHandle dev_null
      }

-- | Converts the floating-point @duration@ to @DiffTime@.
-- The standard function in @Data.Time.Clock@ takes an @Integer@.
secondsToDiffTime :: Double -> DiffTime
secondsToDiffTime = picosecondsToDiffTime . floor . (* picosecondsInSecond)
  where picosecondsInSecond = 1e12

-- | Formats the duration in seconds to a more human-readable format.
formatDuration :: DiffTime -> String
formatDuration = formatTime defaultTimeLocale "%dd %02H:%02M:%02S"
