module Main (main) where

import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Version (showVersion)
import Options.Applicative ((<**>), execParser, fullDesc, helper, info, progDesc)
import System.Directory (getHomeDirectory)
import System.FilePath.Posix ((</>))
import UnliftIO.Async (pooledMapConcurrently)

import Action (Action(..), actionParser)
import AudioDuration (AudioDuration(..))
import FileDurationCacheM (withFileDurationCache)
import GPodderDatabase (getNewEpisodes, getPodcasts, withDatabase)
import GetDuration (getDuration)
import NoDurationCacheM (withoutDurationCache)
import Paths_podtime (version)
import PureParserDurationM (runPureParserDuration)
import Stat (EpisodeCount, mkStat, printStats, recordStat)

main :: IO ()
main = do
  action <- execParser $ info
    (actionParser <**> helper)
    ( fullDesc
    <> progDesc "Prints the total duration of new podcast episodes in gPodder"
    )
  run action

-- | The main function of the program, runs the action requested by the user.
run :: Action -> IO ()
run PrintAllDurations = recordAndLogStats
run (PrintFileDuration file) = printFileDuration file
run PrintVersion = putStrLn . showVersion $ version

-- | Print the duration of a single file.
printFileDuration :: FilePath -> IO ()
printFileDuration file = do
  duration <- runPureParserDuration . withoutDurationCache $ getDuration file
  print $ getAudioDuration duration

-- | Calculates the total duration of the new episodes, appends a stat line to
-- the log file, and prints up to 5 last stat lines from the log.
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

  -- TODO return the file presense check?
  durations <- runPureParserDuration . withFileDurationCache .
    pooledMapConcurrently getDuration $ fmap (gPodderDownloads </>) episodes
  pure (sum durations, fromIntegral $ length episodes)

-- | Measures the wall time duration of running the action `a`.
measure :: IO a -> IO (a, NominalDiffTime)
measure a = do
  start <- getCurrentTime
  x <- a
  end <- getCurrentTime
  pure (x, end `diffUTCTime` start)
