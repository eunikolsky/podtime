module Main (main) where

import Conduit ((.|), runConduitRes, sourceFile)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Version (showVersion)
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))
import UnliftIO.Async (pooledMapConcurrently)

import GPodderDatabase (getNewEpisodes, getPodcasts, withDatabase)
import MP3 (AudioDuration(..), mp3Parser)
import Paths_podtime (version)
import Stat (EpisodeCount, mkStat, printStats, recordStat)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-v"] -> putStrLn . showVersion $ version
    [file] -> getDuration file >>= print . getAudioDuration
    _ -> recordAndLogStats

-- | Returns the audio duration of a single MP3 file. Throws an IO exception
-- if parsing failed.
-- TODO return an error instead
getDuration :: FilePath -> IO AudioDuration
getDuration file = runConduitRes $ sourceFile file .| sinkParser mp3Parser

-- | The main function of the program: calculates the total duration of the new
-- episodes, appends a stat line to the log file, and prints stat lines from the
-- log.
recordAndLogStats :: IO ()
recordAndLogStats = do
  (total, duration) <- measure getTotalDuration
  stat <- mkStat total duration
  recordStat stat
  printStats

-- | Calculates the total duration of new podcast episodes in the gPodder
-- database. Returns the duration and the number of episodes contributed there.
getTotalDuration :: IO (AudioDuration, EpisodeCount)
getTotalDuration = do
  homeDir <- getHomeDirectory
  let gPodderHome = homeDir </> "gPodder"
      gPodderDownloads = gPodderHome </> "Downloads"
  -- TODO stream through conduit
  episodes <- withDatabase (gPodderHome </> "Database") $ do
    podcasts :: [Int] <- getPodcasts
    episodeLists :: [[FilePath]] <- traverse getNewEpisodes podcasts
    pure $ concat episodeLists

  -- FIXME return the file presense check?
  durations <- pooledMapConcurrently getDuration $ fmap (gPodderDownloads </>) episodes
  pure (sum durations, fromIntegral $ length episodes)

-- | Measures the wall time duration of running the action `a`.
measure :: IO a -> IO (a, NominalDiffTime)
measure a = do
  start <- getCurrentTime
  x <- a
  end <- getCurrentTime
  pure (x, end `diffUTCTime` start)
