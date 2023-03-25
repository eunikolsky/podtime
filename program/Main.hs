module Main (main) where

import Conduit ((.|), runConduitRes, sourceFile)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Version (showVersion)
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))
import UnliftIO.Async (pooledMapConcurrently)

import GPodderDatabase (getNewEpisodes, getPodcasts, withDatabase)
import MP3 (AudioDuration(..), mp3Parser)
import Paths_podtime (version)
import Stat (mkStat, printStats, recordStat)

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
  total <- getTotalDuration
  stat <- mkStat total
  recordStat stat
  printStats

-- | Calculates the total duration of new podcast episodes in the gPodder database.
getTotalDuration :: IO AudioDuration
getTotalDuration = do
  homeDir <- getHomeDirectory
  let gPodderHome = homeDir </> "gPodder"
      gPodderDownloads = gPodderHome </> "Downloads"
  episodes <- withDatabase (gPodderHome </> "Database") $ do
    podcasts :: [Int] <- getPodcasts
    episodeLists :: [[FilePath]] <- traverse getNewEpisodes podcasts
    pure $ concat episodeLists

  -- FIXME return the file presense check?
  durations <- pooledMapConcurrently getDuration $ fmap (gPodderDownloads </>) episodes
  pure $ sum durations
