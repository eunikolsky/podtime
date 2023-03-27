module GetDurationSpec (spec) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import GetDuration
import MP3
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec =
  describe "getDuration" $ do
    context "when cached duration is missing" $ do
      prop "returns calculated duration" $
        \(PrintableString fp) (PositiveAudioDuration duration) -> do
          let durations = M.singleton fp duration
              cachedDurations = CachedDurationMap M.empty
          runTestDurationM (getDuration fp) durations cachedDurations `shouldBe` duration

      prop "caches calculated duration" $
        \(PrintableString fp) (PositiveAudioDuration duration) -> do
          let durations = M.singleton fp duration
              cachedDurations = CachedDurationMap M.empty
              expectedCachedDurations = CachedDurationMap durations
          execTestDurationM (getDuration fp) durations cachedDurations `shouldBe` expectedCachedDurations

    context "when cached duration is present" $ do
      prop "returns cached duration" $
        \(PrintableString fp) (PositiveAudioDuration duration) -> do
          let durations = M.empty
              cachedDurations = CachedDurationMap $ M.singleton fp duration
          runTestDurationM (getDuration fp) durations cachedDurations `shouldBe` duration

      prop "doesn't calculate duration" $
        \(PrintableString fp) (PositiveAudioDuration duration) -> do
          -- this injects the `undefined` value, which will throw an error if
          -- calculation is requested
          let durations = M.singleton fp undefined
              cachedDurations = CachedDurationMap $ M.singleton fp duration
          runTestDurationM (getDuration fp) durations cachedDurations `shouldBe` duration

-- | Map from a filename to its audio duration. It's used to mock the duration
-- calculation instead of real parsing (`MonadDuration m`).
type DurationMap = Map FilePath AudioDuration

-- | Map from a filename to its cached audio duration. It's used to mock a
-- durations cache (`MonadDurationCache m`).
newtype CachedDurationMap = CachedDurationMap { getCachedDurationMap :: DurationMap }
  deriving newtype (Show, Eq)

-- | The monad to provide mock duration data in order to test `getDuration` in a
-- pure way.
newtype TestDurationM a = TestDurationM (ReaderT DurationMap (State CachedDurationMap) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader DurationMap, MonadState CachedDurationMap)

instance MonadDuration TestDurationM where
  calculateDuration fp = asks $ M.findWithDefault 0 fp

instance MonadDurationCache TestDurationM where
  getCachedDuration fp = gets $ M.lookup fp . getCachedDurationMap
  cacheDuration fp duration = modify' $ CachedDurationMap . M.insert fp duration . getCachedDurationMap

-- | Runs the given action in the test monad with the provided mock data and
-- returns the actions' result.
runTestDurationM :: TestDurationM AudioDuration -> DurationMap -> CachedDurationMap -> AudioDuration
runTestDurationM (TestDurationM a) durations cachedDurations = flip evalState cachedDurations $ runReaderT a durations

-- | Runs the given action in the test monad with the provided mock data and
-- returns the final cached durations.
execTestDurationM :: TestDurationM AudioDuration -> DurationMap -> CachedDurationMap -> CachedDurationMap
execTestDurationM (TestDurationM a) durations cachedDurations = flip execState cachedDurations $ runReaderT a durations

newtype PositiveAudioDuration = PositiveAudioDuration AudioDuration
  deriving newtype (Show)

instance Arbitrary PositiveAudioDuration where
  arbitrary = PositiveAudioDuration . AudioDuration . getPositive <$> arbitrary
