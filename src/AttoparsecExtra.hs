module AttoparsecExtra
  ( getPos
  , skipUpTo
  , takeUpTo
  ) where

import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.Internal.Types qualified as AI (Parser(..), fromPos)
import Data.ByteString (ByteString)
import Data.Word

-- | Consumes up to `n` bytes of input. Returns everything consumed so far if
-- EOF is encountered; if nothing is consumed before EOF, returns an empty
-- string. An "extended" version of `attoparsec`'s `take` combinator.
takeUpTo :: Int -> Parser ByteString
takeUpTo = skipUpTo (const True)

-- | Consumes up to `n` bytes of input satisfying the predicate. Returns
-- everything consumed so far if EOF is encountered; if nothing is consumed
-- before EOF, returns an empty string. An "extended" version of `attoparsec`'s
-- `skipWhile` combinator.
skipUpTo :: (Word8 -> Bool) -> Int -> Parser ByteString
skipUpTo p n = A.scan 0 (\len byte -> if len >= n || not (p byte) then Nothing else Just (len + 1))

-- | Retrieves the current position.
--
-- Note: this uses private API to get this data, but it should be fine for now.
-- Source: https://github.com/haskell/attoparsec/issues/101
getPos :: AI.Parser i Int
getPos = AI.Parser $ \t pos more _ succ' -> succ' t pos more (AI.fromPos pos)
