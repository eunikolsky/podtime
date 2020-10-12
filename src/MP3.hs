{-# LANGUAGE OverloadedStrings #-}

module MP3 where

import qualified Data.ByteString.Lazy as BL
import           Data.Functor
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Byte

type Parser = Parsec Void BL.ByteString

type ParserBL = Parser BL.ByteString

type Frame = ()

-- | Parses the start of an MP3 frame header, assuming MPEG-1 Layer 3
-- without error protection.
headerStart :: ParserBL
headerStart = string "\xff\xfb"

-- | Parses an MP3 stream.
mp3Parser :: Parser [Frame]
mp3Parser = do
  optional id3Parser
  headerStart $> [()]

  where
    id3Parser = do
      void $ string "ID3"
      skipMany anySingle

newtype BitRate = BitRate { unBitRate :: Int }
newtype SampleRate = SampleRate { unSampleRate :: Int }
newtype Padding = Padding { unPadding :: Bool }

-- http://www.multiweb.cz/twoinches/MP3inside.htm

-- | @frameLen@ includes frame header (4 bytes).
frameLen :: BitRate -> SampleRate -> Padding -> Int
frameLen br sr p = floor $ (144.0 * (fromIntegral (unBitRate br) :: Double) / (fromIntegral (unSampleRate sr) :: Double)) + padding
  where padding = if unPadding p then 1 else 0
