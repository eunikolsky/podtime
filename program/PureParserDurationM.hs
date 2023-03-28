module PureParserDurationM
  ( runPureParserDuration
  ) where

import Conduit ((.|), MonadIO, MonadThrow, MonadTrans, MonadUnliftIO, lift, runConduitRes, sourceFile)
import Control.Monad.Identity (IdentityT(runIdentityT))
import Data.Conduit.Attoparsec (sinkParser)
import GetDuration (MonadDuration(..), MonadModTime(..))
import MP3 (mp3Parser)

-- | Implements the `MonadDuration` interface by using the `mp3Parser`.
--
-- Note: the `IdentityT` is only necessary to implement the `MonadTrans` class
-- to allow to `lift` the `getModTime` into this monad.
newtype PureParserDurationM m a = PureParserDurationM (IdentityT m a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadTrans, MonadIO, MonadUnliftIO, MonadThrow
    )

runPureParserDuration :: PureParserDurationM m a -> m a
runPureParserDuration (PureParserDurationM a) = runIdentityT a

instance (MonadUnliftIO m, MonadThrow m) => MonadDuration (PureParserDurationM m) where
  calculateDuration file = runConduitRes $ sourceFile file .| sinkParser mp3Parser

instance MonadModTime m => MonadModTime (PureParserDurationM m) where
  getModTime = lift . getModTime
