module Main (main) where

import Conduit
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently)
import Control.Monad (filterM)
import Data.Conduit.Attoparsec
import Data.List (genericLength)
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Version (showVersion)
import Database.SQLite.Simple
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))
import System.IO (IOMode(..), withBinaryFile)
import System.Process (StdStream(..), cwd, proc, readCreateProcess, std_err)

import Database
import MP3 qualified
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
  d <- runConduitRes $ sourceFile file .| sinkParser MP3.mp3Parser
  print $ MP3.getAudioDuration d

-- | The main function of the program: calculates and prints the total
-- duration of the new episodes.
printTotalDuration :: Int -> IO ()
printTotalDuration caps = do
  homeDir <- getHomeDirectory
  let gPodderHome = homeDir </> "gPodder"
  allEpisodes <- withConnection (gPodderHome </> "Database") $ \conn -> do
    podcasts <- getPodcasts conn
    fmap concat . traverse (getNewEpisodes conn) $ podcasts

  let episodeGroups = subgroups (ceiling @Double $ genericLength allEpisodes / fromIntegral caps) allEpisodes
  durations <- forConcurrently episodeGroups $ sumPodcastDurations (gPodderHome </> "Downloads")
  putStrLn . formatDuration . secondsToDiffTime . sum $ durations

{-
 podcasts :: [Int]
 fmap getNewEpisodes podcasts :: [IO [String]]
 traverse getNewEpisodes podcasts :: IO [[String]]
 fmap join . traverse getNewEpisodes $ podcasts :: IO [String]
 -}

-- | Splits the list @xs@ into subgroups such that each one contains
-- @maxLen@ items (the last one may contain fewer items).
subgroups :: Int -> [a] -> [[a]]
subgroups _ [] = []
subgroups maxLen xs =
  let (group, rest) = splitAt maxLen xs
  in (group : subgroups maxLen rest)

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

-- | Converts the floating-point @duration@ to @DiffTime@.
-- The standard function in @Data.Time.Clock@ takes an @Integer@.
secondsToDiffTime :: Double -> DiffTime
secondsToDiffTime = picosecondsToDiffTime . floor . (* picosecondsInSecond)
  where picosecondsInSecond = 1e12

-- | Formats the duration in seconds to a more human-readable format.
formatDuration :: DiffTime -> String
formatDuration = formatTime defaultTimeLocale "%dd %02H:%02M:%02S"
