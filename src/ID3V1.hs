module ID3V1
  ( id3Parser
  ) where

import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A

-- | Parses the ID3 v1 tag.
id3Parser :: Parser ()
id3Parser = void $ A.string "TAG"
