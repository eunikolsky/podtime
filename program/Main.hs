module Main (main) where

import Conduit ((.|), runConduitRes, sourceFile)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently)
import Control.Monad (filterM)
import Data.Conduit.Attoparsec (sinkParser)
import Data.List (genericLength)
import Data.Version (showVersion)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))
import System.IO (IOMode(..), withBinaryFile)
import System.Process (StdStream(..), cwd, proc, readCreateProcess, std_err)

import Database (getNewEpisodes, getPodcasts, withDatabase)
import Lib (formatDuration, secondsToDiffTime, subgroups)
import MP3 (mp3Parser, getAudioDuration)
import Paths_podtime (version)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-v"] -> putStrLn . showVersion $ version
    [file] -> printDuration file
    _ -> getNumCapabilities >>= printTotalDuration

-- | Print the audio duration of a single MP3 file.
printDuration :: FilePath -> IO ()
printDuration file = do
  d <- runConduitRes $ sourceFile file .| sinkParser mp3Parser
  print $ getAudioDuration d

-- | The main function of the program: calculates and prints the total
-- duration of the new episodes.
printTotalDuration :: Int -> IO ()
printTotalDuration caps = do
  homeDir <- getHomeDirectory
  let gPodderHome = homeDir </> "gPodder"
  allEpisodes <- withDatabase (gPodderHome </> "Database") $ do
    podcasts <- getPodcasts
    fmap concat . traverse getNewEpisodes $ podcasts

  let episodeGroups = subgroups (ceiling @Double $ genericLength allEpisodes / fromIntegral caps) allEpisodes
  durations <- forConcurrently episodeGroups $ sumPodcastDurations (gPodderHome </> "Downloads")
  putStrLn . formatDuration . secondsToDiffTime . sum $ durations

{-
 podcasts :: [Int]
 fmap getNewEpisodes podcasts :: [IO [String]]
 traverse getNewEpisodes podcasts :: IO [[String]]
 fmap join . traverse getNewEpisodes $ podcasts :: IO [String]
 -}

-- | Retrieves the durations of the podasts at the @paths@ (using `sox`)
-- and sums them up. The @paths@ should be relative to @gPodderDownloads@;
-- missing files are skipped.
sumPodcastDurations :: FilePath -> [String] -> IO Double
sumPodcastDurations gPodderDownloads paths = do
  existingPaths <- filterM (doesFileExist . (gPodderDownloads </>)) paths
  withBinaryFile "/dev/null" WriteMode $ \dev_null -> do
    stdout <- readCreateProcess (sox existingPaths dev_null) ""
    return . sum . fmap read . lines $ stdout

  where
    sox existingPaths dev_null = (proc "sox" (["--info", "-D"] <> existingPaths))
      { cwd = Just gPodderDownloads
      , std_err = UseHandle dev_null
      }
