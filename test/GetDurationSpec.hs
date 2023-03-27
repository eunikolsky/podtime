module GetDurationSpec (spec) where

import Control.Monad.Reader
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
    prop "returns calculated duration" $
      \(PrintableString fp) (PositiveAudioDuration duration) -> do
        let durations = M.singleton fp duration
            cachedDurations = CachedDurationMap M.empty
        runTestDurationM (getDuration fp) durations cachedDurations `shouldBe` duration

    prop "returns cached duration" $
      \(PrintableString fp) (PositiveAudioDuration duration) -> do
        let durations = M.empty
            cachedDurations = CachedDurationMap $ M.singleton fp duration
        runTestDurationM (getDuration fp) durations cachedDurations `shouldBe` duration

-- | Map from a filename to its audio duration. It's used to mock the duration
-- calculation instead of real parsing (`MonadDuration m`).
type DurationMap = Map FilePath AudioDuration

-- | Map from a filename to its cached audio duration. It's used to mock a
-- durations cache (`MonadDurationCache m`).
newtype CachedDurationMap = CachedDurationMap DurationMap

-- | The monad to provide mock duration data in order to test `getDuration` in a
-- pure way.
newtype TestDurationM a = TestDurationM (Reader (DurationMap, CachedDurationMap) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader (DurationMap, CachedDurationMap))

instance MonadDuration TestDurationM where
  calculateDuration fp = asks $ M.findWithDefault 0 fp . fst

instance MonadDurationCache TestDurationM where
  getCachedDuration fp = do
    (CachedDurationMap durations) <- asks snd
    pure $ M.lookup fp durations

-- | Runs the given action in the test monad with the provided mock data.
runTestDurationM :: TestDurationM AudioDuration -> DurationMap -> CachedDurationMap -> AudioDuration
runTestDurationM (TestDurationM a) durations cachedDurations = runReader a (durations, cachedDurations)

newtype PositiveAudioDuration = PositiveAudioDuration AudioDuration
  deriving newtype (Show)

instance Arbitrary PositiveAudioDuration where
  arbitrary = PositiveAudioDuration . AudioDuration . getPositive <$> arbitrary
