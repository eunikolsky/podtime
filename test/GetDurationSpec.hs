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
        runTestDurationM (getDuration fp) durations `shouldBe` duration

newtype TestDurationM a = TestDurationM (Reader (Map FilePath AudioDuration) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader (Map FilePath AudioDuration))

instance MonadDuration TestDurationM where
  calculateDuration fp = asks $ M.findWithDefault 0 fp

runTestDurationM :: TestDurationM AudioDuration -> Map FilePath AudioDuration -> AudioDuration
runTestDurationM (TestDurationM a) = runReader a

newtype PositiveAudioDuration = PositiveAudioDuration AudioDuration
  deriving newtype (Show)

instance Arbitrary PositiveAudioDuration where
  arbitrary = PositiveAudioDuration . AudioDuration . getPositive <$> arbitrary
