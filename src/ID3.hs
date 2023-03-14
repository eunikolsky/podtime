module ID3
  ( id3Parser
  ) where

import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Bits
import Data.Foldable
import Data.Functor
import Data.Word

-- | Parses an ID3 v2.4 tag.
id3Parser :: Parser ()
id3Parser = do
  _ <- A.string "ID3\x04\x00\x00"
  synchsafeSizeBytes <- A.count 4 A.anyWord8
  let size = unSynchsafe synchsafeSizeBytes
  _ <- A.take $ fromIntegral size
  pure ()

-- | Converts a 28-bit synchsafe integer (in 4 bytes) to a proper 28-bit integer,
-- that is dropping the most significant bit of every byte and "skipping" those
-- removed bits. For example: `383 (0b00000001_01111111)` => `255 (0b11111111)`.
--
-- Read more in section "6.2.   Synchsafe integers" at
-- https://github.com/id3/ID3v2.4/blob/master/id3v2.40-structure.txt.
unSynchsafe :: [Word8] -> Word32
unSynchsafe bytes = orWords shiftedWords
  where
    shiftedWords :: [Word32]
    shiftedWords = ([0..3] <&>) $ \index ->
      let bitShift = (3 - index) * 7
      in fromIntegral (bytes !! index) `shiftL` bitShift

    orWords :: [Word32] -> Word32
    orWords = getIor . foldMap' Ior
