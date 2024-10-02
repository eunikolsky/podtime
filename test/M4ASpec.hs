module M4ASpec (spec) where

import M4A
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Builder qualified as BSB
import Data.Foldable
import Prelude hiding (pred)
import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "m4aParser" $ do
    it "parses a sample file" $ do
      let file = mconcat
                  -- this structure is based on a sample podcast file
                  [ box "ftyp" "M4A \x00\x00\x00\x00isomiso2"
                  , box "free" mempty
                  , box "mdat" . zeros $ 0x1971d31 - 8
                  , box "moov" $ subbox (0x465e4 - 8)
                    [ box "mvhd" -- 0x6c - 8 bytes
                      "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                      \\x00\x00\x03\xe8\
                      \\x00\x19\x71\x90\
                      \\x00\x01\x00\x00\x01\x00\x00\x00\x00\x00\
                      \\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\
                      \\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\
                      \\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\
                      \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                      \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02"
                    , box "trak" . zeros $ 0x4650e - 8
                    -- other boxes are omitted
                    ]
                  ]
          expected = 1 / 0x03e8 * 0x197190
      file ~> m4aParser `shouldBe` Right expected

-- Creates an mp4 box with the given name and content. The box's size is calculated automatically.
box :: ByteString -> ByteString -> ByteString
box name content = mconcat
  [ BSL.toStrict . BSB.toLazyByteString . BSB.word32BE . fromIntegral $ BS.length name + BS.length content + 4
  , name
  , content
  ]

subbox :: Int -> [ByteString] -> ByteString
subbox fullSize boxes = mconcat boxes <> filler
  where filler = zeros $ fullSize - sum (BS.length <$> boxes)

zeros :: Int -> ByteString
zeros = flip BS.replicate 0
