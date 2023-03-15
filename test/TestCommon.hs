module TestCommon
  ( shouldFailWithErrorContaining
  ) where

import Test.Hspec

-- | Checks that parsing result is a failure containing the given string.
--
-- > input ~> parser `shouldFailWithErrorContaining` "foo"
shouldFailWithErrorContaining :: Show a => Either String a -> String -> Expectation
Left err `shouldFailWithErrorContaining` expected = err `shouldContain` expected
Right parsed `shouldFailWithErrorContaining` _ = expectationFailure $ "Unexpectedly parsed " <> show parsed
