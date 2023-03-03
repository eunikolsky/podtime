module MP3
  ( frameParser
  ) where

import Data.Attoparsec.ByteString

frameParser :: Parser ()
frameParser = pure ()
