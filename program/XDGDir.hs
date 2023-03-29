module XDGDir
  ( XDGDir(..)
  , getXDGPath
  ) where


import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory (XdgDirectory(..))
import System.FilePath ((</>))
import System.Posix.Files (ownerModes, setFileMode)
import System.Posix.Types (FileMode)
import UnliftIO.Directory (createDirectory, doesDirectoryExist, getXdgDirectory)

-- | Supported XDG base directories.
data XDGDir = XDGCache | XDGData

xdgDirectory :: XDGDir -> XdgDirectory
xdgDirectory XDGCache = XdgCache
xdgDirectory XDGData = XdgData

-- | Returns the path to `file` in the program subdirectory in the given
-- `XDGDir` directory. Creates the subdirectory if missing.
getXDGPath :: MonadIO m => XDGDir -> FilePath -> m FilePath
getXDGPath xdgDir file = do
  dir <- getXdgDirectory (xdgDirectory xdgDir) programName
  dirExists <- doesDirectoryExist dir
  unless dirExists $
    createDirectory dir >> liftIO (setFileMode dir userRWX)
  pure $ dir </> file

programName :: FilePath
programName = "podtime"

-- | Mode `700` for the created cache subdirectory, as suggested by the
-- XDG Base Directory Specification.
-- https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
userRWX :: FileMode
userRWX = ownerModes
