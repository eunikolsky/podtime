{-# LANGUAGE QuantifiedConstraints #-}

module LiftMonadDurationT
  ( LiftMonadDurationT(..)
  ) where

import Control.Monad.Trans (MonadTrans, lift)
import GetDuration (MonadDuration(..))

-- | This `newtype` is only to provide the implementation of `MonadDuration`
-- that lifts `calculateDuration` into an inner `MonadDuration` monad.
--
-- See also: https://kowainik.github.io/posts/deriving#via
newtype (forall i. Monad i => Monad (t i)) => LiftMonadDurationT t m a = LiftMonadDurationT (t m a)
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance (MonadDuration m, MonadTrans t, Monad (t m)) => MonadDuration (LiftMonadDurationT t m) where
  calculateDuration = lift . calculateDuration
