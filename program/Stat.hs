module Stat
  ( mkStat
  , printStats
  , recordStat
  ) where

import Conduit ((.|), runConduitRes, sourceFile)
import Data.Conduit.Combinators (stdout)
import Data.Text (Text)
import Data.Text.IO qualified as T (appendFile)
import Lib (formatDuration)
import MP3 (AudioDuration)
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.FilePath ((</>))

-- | The result of running the program, interesting to the user.
newtype Stat = Stat { duration :: AudioDuration }

-- | Renders a `Stat` value as `Text`.
showStat :: Stat -> Text
showStat = formatDuration . duration

-- | Creates a `Stat` value.
mkStat :: AudioDuration -> Stat
mkStat duration = Stat { duration }

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
