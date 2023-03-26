module TestCommon
  ( complete
  , noMoreThan
  , shouldFailWithErrorContaining
  ) where

import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Test.Hspec

-- | Checks that parsing result is a failure containing the given string.
--
-- > input ~> parser `shouldFailWithErrorContaining` "foo"
shouldFailWithErrorContaining :: Show a => Either String a -> String -> Expectation
Left err `shouldFailWithErrorContaining` expected = err `shouldContain` expected
Right parsed `shouldFailWithErrorContaining` _ = expectationFailure $ "Unexpectedly parsed " <> show parsed

-- | Parser combinator to make sure the entire input is consumed.
complete :: Parser a -> Parser a
complete = (<* A.endOfInput)

-- | Returns the first number if it's <= the second number; otherwise, the second
-- number. It's a more obvious name for `min`.
noMoreThan :: Ord a => a -> a -> a
noMoreThan = min
