module Main (main) where

import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Version (showVersion)
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))
import UnliftIO.Async (pooledMapConcurrently)

import Duration (withCachedDuration)
import GPodderDatabase (getNewEpisodes, getPodcasts, withDatabase)
import GetDuration (getDuration)
import MP3 (AudioDuration(..))
import Paths_podtime (version)
import PureParserDurationM (runPureParserDuration)
import Stat (EpisodeCount, mkStat, printStats, recordStat)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-v"] -> putStrLn . showVersion $ version
    [file] -> printFileDuration file
    _ -> recordAndLogStats

-- | Print the duration of a single file.
printFileDuration :: FilePath -> IO ()
printFileDuration file = do
  -- FIXME this action should not use cache!
  duration <- runPureParserDuration . withCachedDuration $ getDuration file
  print $ getAudioDuration duration

-- | The main function of the program: calculates the total duration of the new
-- episodes, appends a stat line to the log file, and prints up to 5 last stat
-- lines from the log.
recordAndLogStats :: IO ()
recordAndLogStats = do
  (total, duration) <- measure getTotalDuration
  stat <- mkStat total duration
  recordStat stat
  printStats 5

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
  durations <- runPureParserDuration . withCachedDuration .
    pooledMapConcurrently getDuration $ fmap (gPodderDownloads </>) episodes
  pure (sum durations, fromIntegral $ length episodes)

-- | Measures the wall time duration of running the action `a`.
measure :: IO a -> IO (a, NominalDiffTime)
measure a = do
  start <- getCurrentTime
  x <- a
  end <- getCurrentTime
  pure (x, end `diffUTCTime` start)
