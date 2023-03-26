module ConduitExtra
  ( tailC
  ) where

import Conduit
import Data.Word

-- | Consumes all values from the stream and returns `n` last ones. If the
-- stream doesn't produce any values, returns an empty list.
tailC :: Word8 -> ConduitT a o m [a]
tailC _ = pure []
