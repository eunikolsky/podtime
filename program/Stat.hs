module Stat
  ( EpisodeCount
  , mkStat
  , printStats
  , recordStat
  ) where

import Conduit ((.|), runConduitRes, sourceFile)
import Data.Conduit.Combinators (stdout)
import Data.Text (Text)
import Data.Text qualified as T (intercalate, pack)
import Data.Text.IO qualified as T (appendFile)
import Data.Time (LocalTime, NominalDiffTime, defaultTimeLocale, formatTime, getZonedTime, zonedTimeToLocalTime)
import Data.Version (Version, showVersion)
import Data.Word (Word16)
import Lib (formatDuration)
import MP3 (AudioDuration)
import Paths_podtime qualified as Paths (version)
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.FilePath ((</>))

-- it's an unsigned integer to indicate that it can't be negative
type EpisodeCount = Word16

-- | The result of running the program, interesting to the user.
data Stat = Stat
  { duration :: !AudioDuration
  -- ^ total duration of new podcast episodes
  , time :: !LocalTime
  -- ^ time when the result was generated
  , episodeCount :: !EpisodeCount
  -- ^ the amount of episodes contributing to the total duration
  , version :: !Version
  -- ^ version of the program that generated the result
  , elapsedDuration :: !NominalDiffTime
  -- ^ wall time duration that it took to calculate the result
  }

-- | Renders a `Stat` value as `Text`.
showStat :: Stat -> Text
showStat (Stat { duration, time, episodeCount, version, elapsedDuration }) = T.intercalate " | "
  [ T.pack $ formatTime defaultTimeLocale "%F %T" time
  , formatDuration duration
  , T.pack $ show episodeCount
  , T.pack $ showVersion version
  , T.pack $ formatTime defaultTimeLocale "%Es" elapsedDuration
  ]

-- | Creates a `Stat` value with the total duration, episode count and elapsed
-- duration.
mkStat :: (AudioDuration, EpisodeCount) -> NominalDiffTime -> IO Stat
mkStat (duration, episodeCount) elapsedDuration = do
  now <- zonedTimeToLocalTime <$> getZonedTime
  pure Stat { duration, time = now, episodeCount, version = Paths.version, elapsedDuration }

-- | Appends the `Stat` value to the program's log file.
recordStat :: Stat -> IO ()
recordStat stat = do
  logFilepath <- getLogFilepath
  T.appendFile logFilepath $ showStat stat <> "\n"

-- | Prints the program's log file, which contains serialized `Stat` values.
printStats :: IO ()
printStats = do
  logFilepath <- getLogFilepath
  runConduitRes $
    -- there is no conduit to source file as `Text` strings, so this uses the
    -- `ByteString` version, which is actually fine for this case
    sourceFile logFilepath .| stdout

-- | Returns the filepath to the log file in the XDG data directory.
getLogFilepath :: IO FilePath
getLogFilepath = do
  dataDir <- getUserDataDir programDir
  let createParents = True
  createDirectoryIfMissing createParents dataDir
  pure $ dataDir </> logFilename

  where
    programDir = "podtime"
    logFilename = "stats"
