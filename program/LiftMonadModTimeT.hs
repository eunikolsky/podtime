{-# LANGUAGE QuantifiedConstraints #-}

module LiftMonadModTimeT
  ( LiftMonadModTimeT(..)
  ) where

import Control.Monad.Trans (MonadTrans, lift)
import GetDuration (MonadModTime(..))

-- | This `newtype` is only to provide the implementation of `MonadModTime`
-- that lifts `getModTime` into an inner `MonadModTime` monad.
--
-- See also: https://kowainik.github.io/posts/deriving#via
newtype (forall i. Monad i => Monad (t i)) => LiftMonadModTimeT t m a = LiftMonadModTimeT (t m a)
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance (MonadModTime m, MonadTrans t, Monad (t m)) => MonadModTime (LiftMonadModTimeT t m) where
  -- note: `lift` used to be `liftIO` before extracting this newtype
  getModTime = lift . getModTime
