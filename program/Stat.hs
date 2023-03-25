module Stat
  ( mkStat
  , printStats
  , recordStat
  ) where

import Conduit ((.|), runConduitRes, sourceFile)
import Data.Conduit.Combinators (stdout)
import Data.Text (Text)
import Data.Text qualified as T (intercalate, pack)
import Data.Text.IO qualified as T (appendFile)
import Data.Time (LocalTime, defaultTimeLocale, formatTime, getZonedTime, zonedTimeToLocalTime)
import Data.Version (Version, showVersion)
import Lib (formatDuration)
import MP3 (AudioDuration)
import Paths_podtime qualified as Paths (version)
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.FilePath ((</>))

-- | The result of running the program, interesting to the user.
data Stat = Stat
  { duration :: !AudioDuration
  -- ^ total duration of new podcast episodes
  , time :: !LocalTime
  -- ^ time when the result was generated
  , version :: !Version
  -- ^ version of the program that generated the result
  }

-- | Renders a `Stat` value as `Text`.
showStat :: Stat -> Text
showStat (Stat { duration, time, version }) = T.intercalate " | "
  [ T.pack $ formatTime defaultTimeLocale "%F %T" time
  , formatDuration duration
  , T.pack $ showVersion version
  ]

-- | Creates a `Stat` value.
mkStat :: AudioDuration -> IO Stat
mkStat duration = do
  now <- zonedTimeToLocalTime <$> getZonedTime
  pure Stat { duration, time = now, version = Paths.version }

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
