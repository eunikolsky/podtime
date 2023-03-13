module ID3
  ( id3Parser
  ) where

import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Functor

-- | Parses an ID3 v2.4 tag.
id3Parser :: Parser ()
id3Parser = A.string "ID3" $> ()
