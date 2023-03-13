module ID3
  ( id3Parser
  ) where

import Data.Attoparsec.ByteString (Parser)

-- | Parses an ID3 v2.4 tag.
id3Parser :: Parser ()
id3Parser = pure ()
