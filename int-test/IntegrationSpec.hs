module IntegrationSpec (main) where

import AudioDuration
import Control.Exception
import Control.Monad
import Data.Attoparsec.Text
import Data.ByteString qualified as B
import Data.Foldable
import Data.List (isInfixOf, partition, sort)
import Data.Maybe
import Data.Text qualified as T
import M4A
import MP3
import Numeric
import SuccessFormatter
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Test.Hspec
import Test.Hspec.Api.Formatters.V1
import Test.Hspec.Attoparsec
import Test.Hspec.Core.Runner
import Text.Read
import Text.Show.Unicode

main :: IO ()
main = do
  maybeDir <- lookupEnv "TEST_DIR"
  maybeFile <- lookupEnv "TEST_FILE"
  episodes <- findEpisodes maybeFile maybeDir
  let (mp3Episodes, m4aEpisodes) = separateEpisodes episodes

  let config = useFormatter ("success", successFormatter) $ defaultConfig
        { configPrintSlowItems = Just 5
        }
  hspecWith config . parallel $ do
    mp3Spec mp3Episodes
    m4aSpec m4aEpisodes

mp3Spec :: Episodes -> Spec
mp3Spec (Episodes baseDir mp3s) =
  describe "mp3Parser" $ do
    forM_ mp3s $ \mp3 -> do
      it ("parses " <> ushow mp3) $ do
        contents <- B.readFile $ baseDir </> mp3
        mp3Parser `shouldSucceedOn` contents

      parallel . fit ("parsed duration matches sox's/ffmpeg's duration: " <> ushow mp3) $ do
        let filepath = baseDir </> mp3
        contents <- B.readFile filepath
        soxDuration <- getSoxDuration filepath
        let result = contents ~> mp3Parser
        case result of
          Left _ -> result `parsesDuration` soxDuration
          Right res ->
            if isNothing $ isDurationCorrect res soxDuration
              then result `parsesDuration` soxDuration
              else do
                -- if we disagree with `sox`'s result, compare the result with
                -- `ffmpeg`'s result
                ffmpegDuration <- getFFMpegDuration filepath
                result `parsesDuration` ffmpegDuration

m4aSpec :: Episodes -> Spec
m4aSpec (Episodes baseDir m4as) =
  describe "m4aParser" $ do
    forM_ m4as $ \m4a -> do
      it ("parses " <> ushow m4a) $ do
        contents <- B.readFile $ baseDir </> m4a
        m4aParser `shouldSucceedOn` contents

      parallel . fit ("parsed duration matches ffmpeg's duration: " <> ushow m4a) $ do
        let filepath = baseDir </> m4a
        contents <- B.readFile filepath
        ffmpegDuration <- getFFMpegDuration filepath
        contents ~> m4aParser `parsesDuration` ffmpegDuration

-- | Checks that the parsed duration equals to the expected duration with the
-- error of at most 0.11 seconds.
parsesDuration :: Either String AudioDuration -> AudioDuration -> Expectation
result `parsesDuration` expected =
  either (expectationFailure . errmsg) checkDuration result

  where
    errmsg err = "expected a parsed duration around " <> show expected
      <> "\nbut parsing failed with error: " <> show err

    checkDuration actual = for_ (isDurationCorrect actual expected) expectationFailure

isDurationCorrect :: AudioDuration -> AudioDuration -> Maybe String
isDurationCorrect actual expected = if abs diff > epsilon then Just err else mempty
  where
    diff = expected - actual
    epsilon = 0.11 :: AudioDuration
    err = mconcat
      [ "parsed duration ", show actual
      , " doesn't match reference duration ", show expected
      , "\nthe difference is ", showFFloat (Just 3) (getAudioDuration diff) ""
      , "s (", showFFloat (Just 3) (getAudioDuration $ diff / expected * 100) ""
      , "%), more than ", show epsilon
      ]

-- | Runs `sox` to get the duration of the mp3 file. The duration is not
-- estimated, but calculated accurately (although `sox` does report an incorrect
-- duration in rare cases).
getSoxDuration :: FilePath -> IO AudioDuration
getSoxDuration mp3 = getDuration <$> runSox
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

-- | Runs `ffmpeg` to get the duration of the mp3/m4a file. The duration is not
-- estimated, but calculated accurately.
getFFMpegDuration :: FilePath -> IO AudioDuration
getFFMpegDuration audio = getDuration <$> runFFMpeg
  where
    runFFMpeg = do
      (exitCode, _, stderr) <- readProcessWithExitCode
        "ffmpeg"
        ["-v", "quiet", "-stats", "-i", audio, "-f", "null", "-", "-nostdin"]
        ""
      when (exitCode /= ExitSuccess) . throwIO .
        AssertionFailed . mconcat $
          [ "ffmpeg returned exit code ", show exitCode
          , ": ", stderr
          ]
      pure stderr

    getDuration output = parseDuration . fromJust . find ("time=" `T.isPrefixOf`) . T.splitOn " " . last . T.splitOn "\r" $ T.pack output

    parseDuration t = either error id . flip parseOnly t $ do
      void $ string "time="
      hours <- fromIntegral @Int <$> decimal
      void $ char ':'
      minutes <- fromIntegral @Int <$> decimal
      void $ char ':'
      seconds <- double
      endOfInput

      pure . AudioDuration $ ((hours * 60) + minutes) * 60 + seconds

-- | Contains a list of episodes relative to the base directory. This separation
-- is necessary in order to shorten the test names.
data Episodes = Episodes
  FilePath -- ^ base directory
  [FilePath] -- ^ episodes

findEpisodes :: Maybe FilePath -> Maybe FilePath -> IO Episodes
findEpisodes maybeFile maybeDir = do
  baseDir <- gPodderDownloadsDir
  episodes <- case maybeFile of
        Just file -> pure [file]
        Nothing -> do
          let maybeFilterByName = case maybeDir of
                Just dir -> fmap (filter (dir `isInfixOf`))
                Nothing -> id
          episodeDirs <- maybeFilterByName . filterM doesDirectoryExist =<< ls baseDir
          files <- sort . join <$> forM episodeDirs ls
          pure $ filter (isSupportedExtension . takeExtension) files
  pure . Episodes baseDir $ makeRelative baseDir <$> episodes

  where isSupportedExtension ext = (ext == ".mp3") || (ext == ".m4a")

separateEpisodes :: Episodes -> (Episodes, Episodes)
separateEpisodes (Episodes baseDir episodes) = (Episodes baseDir mp3s, Episodes baseDir m4as)
  where (mp3s, m4as) = partition (\ep -> takeExtension ep == ".mp3") episodes

-- | Lists items in the given directory like `listDirectory`, but returns
-- full paths.
ls :: FilePath -> IO [FilePath]
ls dir = fmap (dir </>) <$> listDirectory dir

gPodderDownloadsDir :: IO FilePath
gPodderDownloadsDir = do
  home <- getHomeDirectory
  pure $ home </> "gPodder/Downloads"
