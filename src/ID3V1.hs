module ID3V1
  ( id3Parser
  ) where

import Data.Attoparsec.ByteString (Parser)

-- | Parses the ID3 v1 tag.
id3Parser :: Parser ()
id3Parser = pure ()
