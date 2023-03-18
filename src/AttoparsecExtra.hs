module AttoparsecExtra
  ( getPos
  ) where

import Data.Attoparsec.Internal.Types qualified as AI (Parser(..), fromPos)

-- | Retrieves the current position.
--
-- Note: this uses private API to get this data, but it should be fine for now.
-- Source: https://github.com/haskell/attoparsec/issues/101
getPos :: AI.Parser i Int
getPos = AI.Parser $ \t pos more _ succ' -> succ' t pos more (AI.fromPos pos)
