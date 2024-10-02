module M4A (m4aParser) where

import AudioDuration
import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word

-- | Parses an M4A (MPEG-4 Audio) file and returns the audio duration. The
-- duration is read from the `mvhd` box inside the `moov` box; everything
-- else is ignored.
m4aParser :: Parser AudioDuration
m4aParser = findBox "moov" . findBox "mvhd" $ do
  void $ A.take 12
  timeScale <- anyWord32be
  duration <- anyWord32be
  pure . AudioDuration $ 1 / fromIntegral timeScale * fromIntegral duration

  where
    findBox btype parse = do
      box <- boxParser
      if boxType box == btype
        then parse
        -- this looks like an infinite loop, but the parser will fail with
        -- "not enough input" if we don't find a `moov` box
        else skipContent box *> findBox btype parse

-- We don't parse the content here because we need to either skip it or parse the boxes
-- inside, in which case the parser should stay at the beginning of the content.
data Box = Box
  { boxType :: !ByteString
  , boxSize :: !Word32
  }

boxParser :: Parser Box
boxParser = do
  boxSize <- anyWord32be
  boxType <- A.take 4
  pure Box{boxType, boxSize}

skipContent :: Box -> Parser ()
skipContent box = void . A.take $ fromIntegral (boxSize box) - 8

anyWord32be :: Parser Word32
anyWord32be = foldIntoWord32 <$> A.take 4
  where foldIntoWord32 = BS.foldl' (\acc byte -> acc `shiftL` 8 .|. fromIntegral byte) 0
