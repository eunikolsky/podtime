module NoDurationCacheM
  ( withoutDurationCache
  ) where

import Control.Monad.Identity (IdentityT(runIdentityT))
import Control.Monad.Trans (MonadTrans, lift)
import GetDuration (MonadDuration(..), MonadDurationCache(..), MonadModTime (..))

-- | Offers no cache for duration.
newtype NoDurationCacheM m a = NoDurationCacheM (IdentityT m a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadTrans
    )

withoutDurationCache :: NoDurationCacheM m a -> m a
withoutDurationCache (NoDurationCacheM a) = runIdentityT a

instance Monad m => MonadDurationCache (NoDurationCacheM m) where
  getCachedDuration = const $ pure Nothing
  cacheDuration = const . const $ pure ()

instance MonadDuration m => MonadDuration (NoDurationCacheM m) where
  calculateDuration = lift . calculateDuration

instance MonadModTime m => MonadModTime (NoDurationCacheM m) where
  getModTime = lift . getModTime
