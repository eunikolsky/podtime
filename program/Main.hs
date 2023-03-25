module Main (main) where

import Conduit ((.|), runConduitRes, sourceFile)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Version (showVersion)
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))
import UnliftIO.Async (pooledMapConcurrently)

import GPodderDatabase (getNewEpisodes, getPodcasts, withDatabase)
import Lib (formatDuration)
import MP3 (AudioDuration(..), mp3Parser)
import Paths_podtime (version)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-v"] -> putStrLn . showVersion $ version
    [file] -> getDuration file >>= print . getAudioDuration
    _ -> printTotalDuration

-- | Returns the audio duration of a single MP3 file. Throws an IO exception
-- if parsing failed.
-- TODO return an error instead
getDuration :: FilePath -> IO AudioDuration
getDuration file = runConduitRes $ sourceFile file .| sinkParser mp3Parser

-- | The main function of the program: calculates and prints the total
-- duration of the new episodes.
printTotalDuration :: IO ()
printTotalDuration = do
  homeDir <- getHomeDirectory
  let gPodderHome = homeDir </> "gPodder"
      gPodderDownloads = gPodderHome </> "Downloads"
  allEpisodes <- withDatabase (gPodderHome </> "Database") $ do
    podcasts :: [Int] <- getPodcasts
    episodeLists :: [[FilePath]] <- traverse getNewEpisodes podcasts
    pure $ concat episodeLists

  -- FIXME return the file presense check?
  durations <- pooledMapConcurrently getDuration $ fmap (gPodderDownloads </>) allEpisodes
  putStrLn . formatDuration . sum $ durations
