module CacheItemCSV
  ( CacheItemCSV(..)
  , fromKeyValue
  , toKeyValue
  ) where

import AudioDuration (AudioDuration(..))
import Data.Csv (FromField(..), FromRecord, ToField(..), ToRecord)
import Data.Text qualified as T (pack, unpack)
import Data.Text.Encoding qualified as TE (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import GetDuration (ModTime)
import OrphanInstances ()

-- | Cache items persisted to CSV file.
data CacheItemCSV = CacheItemCSV !FilePath !ModTimeCSV !AudioDuration
  deriving stock Generic

instance FromRecord CacheItemCSV
instance ToRecord CacheItemCSV

fromKeyValue :: ((FilePath, ModTime), AudioDuration) -> CacheItemCSV
fromKeyValue ((fp, mtime), dur) = CacheItemCSV fp (ModTimeCSV mtime) dur

toKeyValue :: CacheItemCSV -> ((FilePath, ModTime), AudioDuration)
toKeyValue (CacheItemCSV fp (ModTimeCSV mtime) dur) = ((fp, mtime), dur)

-- | Wrapper around `ModTime` (aka `UTCTime`) to allow to persist it as a CSV
-- field using the default `Show` and `Read` instances.
newtype ModTimeCSV = ModTimeCSV ModTime
  deriving newtype (Show, Read)

instance FromField ModTimeCSV where
  parseField = pure . read . T.unpack . TE.decodeUtf8

instance ToField ModTimeCSV where
  toField = TE.encodeUtf8 . T.pack . show
