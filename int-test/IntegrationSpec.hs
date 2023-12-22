module IntegrationSpec (main) where

import Control.Exception
import Control.Monad
import Data.ByteString qualified as B
import Data.List (find)
import Data.Maybe
import MP3
import Numeric
import SuccessFormatter
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.Core.Runner
import Text.Read
import Text.Show.Unicode

main :: IO ()
main = do
  episodes <- findEpisodes

  let config = defaultConfig
        { configPrintSlowItems = Just 5
        , configFormatter = Just successFormatter
        }
  hspecWith config . parallel $ spec episodes

spec :: Episodes -> Spec
spec (Episodes baseDir mp3s) =
  describe "mp3Parser" $ do
    forM_ mp3s $ \mp3 -> do
      it ("parses " <> ushow mp3) $ do
        contents <- B.readFile $ baseDir </> mp3
        mp3Parser `shouldSucceedOn` contents

      parallel . fit ("parsed duration matches sox's duration: " <> ushow mp3) $ do
        let filepath = baseDir </> mp3
        contents <- B.readFile filepath
        externalDuration <- getExternalAudioDuration filepath
        contents ~> mp3Parser `parsesDuration` externalDuration

-- | Checks that the parsed duration equals to the expected duration with the
-- error of at most 0.1 seconds.
parsesDuration :: Either String AudioDuration -> AudioDuration -> Expectation
result `parsesDuration` expected =
  either (expectationFailure . errmsg) checkDuration result

  where
    errmsg err = "expected a parsed duration around " <> show expected
      <> "\nbut parsing failed with error: " <> show err

    checkDuration actual =
      let diff = expected - actual
          epsilon = 0.1 :: AudioDuration
      in when (abs diff > epsilon) . expectationFailure $ mconcat
        [ "parsed duration ", show actual
        , " doesn't match reference duration ", show expected
        , "\nthe difference is ", showFFloat (Just 3) (getAudioDuration diff) ""
        , "s (", showFFloat (Just 3) (getAudioDuration $ diff / expected * 100) ""
        , "%), more than ", show epsilon
        ]

-- | Runs `sox` to get the duration of the mp3 file. The duration is not
-- estimated, but calculated accurately.
getExternalAudioDuration :: FilePath -> IO AudioDuration
getExternalAudioDuration mp3 = getDuration <$> runSox
  where
    runSox = do
      (exitCode, _, stderr) <- readProcessWithExitCode
        "sox"
        ["--ignore-length", mp3, "-n", "stat"]
        ""
      when (exitCode /= ExitSuccess) . throwIO .
        AssertionFailed . mconcat $
          [ "sox returned exit code ", show exitCode
          , ": ", stderr
          ]
      pure stderr

    getDuration output = fromJust $ do
      lengthWords <- find ((== "Length") . head) . fmap words $ lines output
      let lengthString = last lengthWords
      AudioDuration <$> readMaybe lengthString

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
