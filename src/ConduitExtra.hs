module ConduitExtra
  ( takeLastC
  ) where

import Conduit
import Data.Conduit.List
import Data.List (genericLength)
import Data.Word

-- | Consumes all values from the stream and forwards only `n` last ones. If the
-- stream doesn't produce any values, forwards no values; if the stream
-- produces fewer than `n` elements, forward them.
takeLastC :: Monad m => Word8 -> ConduitT a a m ()
takeLastC n = loop []
  where
    loop :: Monad m => [a] -> ConduitT a a m ()
    loop !xs = do
      x <- await
      case x of
        Just x' ->
          -- append the new value to the accumulator, dropping the head if over the limit
          let hasMaximum = genericLength xs == n
              limit = if hasMaximum then tail else id
          -- this is effectively a foldl
          in loop $ limit xs <> [x']

        Nothing ->
          -- no more incoming values, so stream the collected values
          sourceList xs
