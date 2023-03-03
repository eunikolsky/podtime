module MP3
  ( frameParser
  ) where

import Data.Attoparsec.ByteString ((<?>), Parser)
import Data.Attoparsec.ByteString qualified as A

frameParser :: Parser ()
frameParser = do
  _ <- A.string "\xff\xfb" <?> "first two header bytes"
  _ <- A.take 2
  let frameSize = 417
      contentsSize = frameSize - 4
  _ <- A.take contentsSize
  pure ()
