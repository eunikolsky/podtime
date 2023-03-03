module MP3Spec where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word
import MP3
import Prelude hiding (pred)
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "frameParser" $ do
    describe "examples" $ do
      it "parses a basic 128 kbps frame" $ do
        let frame = mkFrame
        frameParser `shouldSucceedOn` frame

      it "fails to parse bytes without correct frame sync" $ do
        let frame = mkFrame `replacingHeadWith` 0x00
        frameParser `shouldFailOn` frame

    describe "properties" $ do
      prop "parses a basic 128 kbps frame with any contents"
        . forAll genFrame $ \frame ->
          frameParser `shouldSucceedOn` frame

      prop "fails to parse bytes with incorrect first byte"
        . forAll genInvalidFrame $ \frame ->
          frameParser `shouldFailOn` frame

      prop "fails to parse incomplete frames"
        . forAll genIncompleteFrame $ \frame ->
          frameParser `shouldFailOn` frame

frameSize, frameHeaderSize, contentsSize :: Int
frameSize = 417
frameHeaderSize = 4
contentsSize = frameSize - frameHeaderSize

-- | A standard 128 kb/s, 44.1 kHz mp3 frame.
mkFrame :: ByteString
mkFrame = header <> contents
  where header = BS.pack [0xff, 0b11111011, 0b10010000, 0b11000100]
        contents = BS.replicate contentsSize 0

-- | Generates a standard 128 kb/s, 44.1 kHz mp3 frame with arbitrary contents.
genFrame :: Gen ByteString
genFrame = do
  let header = BS.pack [0xff, 0b11111011, 0b10010000, 0b11000100]
  contents <- vectorOf contentsSize arbitrary
  pure $ header <> BS.pack contents

-- | Generates an invalid mp3 frame where the first byte is incorrect.
genInvalidFrame :: Gen ByteString
genInvalidFrame = do
  -- `firstByte <- arbitrary; when (firstByte == 0xff) discard` didn't discard
  -- the first `0xff` byte (seed 181316514)

  firstByte <- firstM (/= 0xff) $ repeat arbitrary
  frame <- genFrame
  pure $ frame `replacingHeadWith` firstByte

-- | Generates an incomplete mp3 frame.
genIncompleteFrame :: Gen ByteString
genIncompleteFrame = do
  let header = BS.pack [0xff, 0b11111011, 0b10010000, 0b11000100]
  incompleteLength <- chooseInt (0, contentsSize - 1)
  contents <- vectorOf incompleteLength arbitrary
  pure $ header <> BS.pack contents

firstM :: (Monad m, Show a) => (a -> Bool) -> [m a] -> m a
firstM pred (x:xs) = do
  x' <- x
  if pred x' then pure x' else firstM pred xs
firstM _ [] = error "Unexpected empty xs in firstM"

replacingHeadWith :: ByteString -> Word8 -> ByteString
replacingHeadWith bs n = BS.singleton n <> BS.tail bs
