module IntegrationSpec (main) where

import Control.Monad
import System.Directory
import System.FilePath
import Test.Hspec

main :: IO ()
main = do
  mp3s <- findEpisodes
  hspec $ spec mp3s

spec :: Episodes -> Spec
spec (Episodes _ mp3s) =
  describe "mp3Parser" $ do
    forM_ (take 3 mp3s) $ \mp3 ->
      it ("parses " <> show mp3)
        pending

-- | Contains a list of episodes relative to the base directory. This separation
-- is necessary in order to shorten the test names.
data Episodes = Episodes
  FilePath -- ^ base directory
  [FilePath] -- ^ episodes

findEpisodes :: IO Episodes
findEpisodes = do
  baseDir <- gPodderDownloadsDir
  episodeDirs <- filterM doesDirectoryExist =<< ls baseDir
  files <- join <$> forM episodeDirs ls
  let mp3s = filter ((== ".mp3") . takeExtension) files
  pure . Episodes baseDir $ makeRelative baseDir <$> mp3s

-- | Lists items in the given directory like `listDirectory`, but returns
-- full paths.
ls :: FilePath -> IO [FilePath]
ls dir = fmap (dir </>) <$> listDirectory dir

gPodderDownloadsDir :: IO FilePath
gPodderDownloadsDir = do
  home <- getHomeDirectory
  pure $ home </> "gPodder/Downloads"
