module GetDurationSpec (spec) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Time.Clock
import GetDuration
import MP3
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

spec :: Spec
spec =
  describe "getDuration" $ do
    context "when cached duration is missing" $ do
      prop "returns calculated duration" $
        \(PrintableString fp) (PositiveAudioDuration duration) -> do
          let durations = M.singleton fp duration
              cachedDurations = CachedDurationMap M.empty
              modTimes = M.empty
          runTestDurationM (getDuration fp) durations modTimes cachedDurations `shouldBe` duration

      prop "caches calculated duration with modtime" $
        \(PrintableString fp) (PositiveAudioDuration duration) modTime -> do
          let durations = M.singleton fp duration
              cachedDurations = CachedDurationMap M.empty
              expectedCachedDurations = CachedDurationMap $ M.singleton (fp, modTime) duration
              modTimes = M.singleton fp modTime
          execTestDurationM (getDuration fp) durations modTimes cachedDurations `shouldBe` expectedCachedDurations

    context "when cached duration with matching modtime is present" $ do
      prop "returns cached duration" $
        \(PrintableString fp) (PositiveAudioDuration duration) modTime -> do
          let durations = M.empty
              cachedDurations = CachedDurationMap $ M.singleton (fp, modTime) duration
              modTimes = M.singleton fp modTime
          runTestDurationM (getDuration fp) durations modTimes cachedDurations `shouldBe` duration

      prop "doesn't calculate duration" $
        \(PrintableString fp) (PositiveAudioDuration duration) modTime -> do
          -- this injects the `undefined` value, which will throw an error if
          -- calculation is requested
          let durations :: DurationMap = M.singleton fp undefined
              cachedDurations = CachedDurationMap $ M.singleton (fp, modTime) duration
              modTimes = M.singleton fp modTime
          runTestDurationM (getDuration fp) durations modTimes cachedDurations `shouldBe` duration

-- | Map from a filename to its audio duration. It's used to mock the duration
-- calculation instead of real parsing (`MonadDuration m`).
type DurationMap = Map FilePath AudioDuration

-- | Map from a filename to its modification time. It's used to mock the
-- retrieval of the mod time instead of real FS access (`MonadModTime m`).
type ModTimeMap = Map FilePath UTCTime

-- | Map from a filename and its modification time to its cached audio duration.
-- It's used to mock a durations cache (`MonadDurationCache m`).
newtype CachedDurationMap = CachedDurationMap { getCachedDurationMap :: Map (FilePath, UTCTime) AudioDuration }
  deriving newtype (Show, Eq)

-- | The monad to provide mock duration data in order to test `getDuration` in a
-- pure way.
newtype TestDurationM a = TestDurationM
  (ReaderT
    (DurationMap, ModTimeMap)
    (State CachedDurationMap)
    a
  )
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (DurationMap, ModTimeMap), MonadState CachedDurationMap
    )

instance MonadDuration TestDurationM where
  calculateDuration fp = asks $ M.findWithDefault 0 fp . fst

instance MonadDurationCache TestDurationM where
  getCachedDuration key = gets $ M.lookup key . getCachedDurationMap
  cacheDuration fp duration = modify' $ CachedDurationMap . M.insert fp duration . getCachedDurationMap

instance MonadModTime TestDurationM where
  getModTime fp = asks $ M.findWithDefault (error err) fp . snd
    where err = "No mocked time for " <> fp

-- | Runs the given action in the test monad with the provided mock data and
-- returns the actions' result.
runTestDurationM :: TestDurationM AudioDuration ->
  DurationMap -> ModTimeMap -> CachedDurationMap ->
  AudioDuration
runTestDurationM (TestDurationM a) durations modTimes cachedDurations =
  flip evalState cachedDurations $ runReaderT a (durations, modTimes)

-- | Runs the given action in the test monad with the provided mock data and
-- returns the final cached durations.
execTestDurationM :: TestDurationM AudioDuration ->
  DurationMap -> ModTimeMap -> CachedDurationMap ->
  CachedDurationMap
execTestDurationM (TestDurationM a) durations modTimes cachedDurations =
  flip execState cachedDurations $ runReaderT a (durations, modTimes)

newtype PositiveAudioDuration = PositiveAudioDuration AudioDuration
  deriving newtype (Show)

instance Arbitrary PositiveAudioDuration where
  arbitrary = PositiveAudioDuration . AudioDuration . getPositive <$> arbitrary
