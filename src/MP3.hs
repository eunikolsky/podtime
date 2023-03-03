module MP3
  ( frameParser
  ) where

import Data.Attoparsec.ByteString

frameParser :: Parser ()
frameParser = do
  _ <- string "\xff\xfb" <?> "first two header bytes"
  pure ()
