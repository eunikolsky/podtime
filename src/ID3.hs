module ID3
  ( id3Parser
  ) where

import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A

-- | Parses an ID3 v2.4 tag.
id3Parser :: Parser ()
id3Parser = do
  _ <- A.string "ID3\x04\x00\x00\x00\x00\x00"
  size <- A.anyWord8
  _ <- A.take $ fromIntegral size
  pure ()
