module PureParserDurationM
  ( runPureParserDuration
  ) where

import AudioDuration (AudioDuration)
import Conduit ((.|), MonadIO, MonadThrow, MonadTrans, MonadUnliftIO, lift, runConduitRes, sourceFile, throwM)
import Control.Monad.Identity (IdentityT(runIdentityT))
import Data.Conduit.Attoparsec (ParseError(..), sinkParserEither)
import DurationParseError (DurationParseError(..))
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
  calculateDuration file = addFilenameToParseError file . runConduitRes $ sourceFile file .| sinkParserEither mp3Parser

-- | Adds the filename to the parse error (if it happens) and rethrows the
-- extended `DurationParseError`. It will still terminate the program, but
-- it will print the filename with the parse error.
addFilenameToParseError :: MonadThrow m => FilePath -> m (Either ParseError AudioDuration) -> m AudioDuration
addFilenameToParseError file me = do
  e <- me
  case e of
    Left err -> throwM $ DurationParseError file err
    Right d -> pure d

instance MonadModTime m => MonadModTime (PureParserDurationM m) where
  getModTime = lift . getModTime
