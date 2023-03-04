module MP3
  ( frameParser
  ) where

import Data.Attoparsec.ByteString ((<?>), Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Bits

frameParser :: Parser ()
frameParser = do
  _ <- A.string "\xff\xfb" <?> "first two header bytes"
  byte2 <- A.anyWord8
  _ <- A.anyWord8

  let paddingSize = if testBit byte2 paddingBitIndex then 1 else 0
      frameSize = 417
      contentsSize = frameSize - 4 + paddingSize

  _ <- A.take contentsSize
  pure ()

-- TODO try https://github.com/stevana/bits-and-bobs
paddingBitIndex :: Int
paddingBitIndex = 1
