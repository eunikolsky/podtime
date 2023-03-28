{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances () where

import Control.Monad.IO.Class (liftIO)
import Data.Csv (FromField(..), ToField(..))
import Data.Text qualified as T (pack, unpack)
import Data.Text.Encoding qualified as TE (decodeUtf8, encodeUtf8)
import GetDuration (MonadModTime(..))
import MP3 (AudioDuration(..))
import System.Directory (getModificationTime)

-- | Orphan instance to load `AudioDuration` from CSV as its `Double` value.
instance FromField AudioDuration where
  parseField = pure . AudioDuration . read . T.unpack . TE.decodeUtf8

-- | Orphan instance to save `AudioDuration` to CSV as its `Double` value.
instance ToField AudioDuration where
  toField = TE.encodeUtf8 . T.pack . show . getAudioDuration

instance MonadModTime IO where
  getModTime = liftIO . getModificationTime
