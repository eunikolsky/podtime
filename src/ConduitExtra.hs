module ConduitExtra
  ( tailC
  ) where

import Conduit
import Data.List (genericLength)
import Data.Word

-- | Consumes all values from the stream and returns `n` last ones. If the
-- stream doesn't produce any values, returns an empty list; if the stream
-- produces fewer than `n` elements, returns them.
tailC :: Monad m => Word8 -> ConduitT a o m [a]
tailC n = foldlC append []
  where
    append :: [a] -> a -> [a]
    append xs x =
      let hasMaximum = genericLength xs == n
          limit = if hasMaximum then tail else id
      in limit xs <> [x]
