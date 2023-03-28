module NoDurationCacheM
  ( withoutDurationCache
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (IdentityT(runIdentityT))
import Control.Monad.Trans (MonadTrans)
import GetDuration (MonadDuration(..), MonadDurationCache(..), MonadModTime (..))
import LiftMonadDurationT (LiftMonadDurationT(..))
import LiftMonadModTimeT (LiftMonadModTimeT(..))

-- | Offers no cache for duration.
newtype NoDurationCacheM m a = NoDurationCacheM (IdentityT m a)
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)
  deriving MonadDuration via (LiftMonadDurationT IdentityT m)
  deriving MonadModTime via (LiftMonadModTimeT IdentityT m)

withoutDurationCache :: NoDurationCacheM m a -> m a
withoutDurationCache (NoDurationCacheM a) = runIdentityT a

instance Monad m => MonadDurationCache (NoDurationCacheM m) where
  getCachedDuration = const $ pure Nothing
  cacheDuration = const . const $ pure ()
