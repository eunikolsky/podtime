module DurationParseError
  ( DurationParseError(..)
  ) where

import Control.Exception (Exception)
import Data.Conduit.Attoparsec (ParseError(..))
import Text.Show.Unicode (ushow)

-- | `ParseError` extended with the source filename information.
data DurationParseError = DurationParseError
  { dpeFilename :: !FilePath
  , dpeError :: !ParseError
  }

instance Show DurationParseError where
  show (DurationParseError{dpeFilename, dpeError}) = mconcat
    $ ["Parse error of file ", ushow dpeFilename, ": "]
    <> showError dpeError

    where
      showError ParseError{errorContexts, errorMessage, errorPosition} =
        [errorMessage, " at position ", show errorPosition] <> nonEmptyContexts errorContexts
      showError DivergentParser = pure "divergent parser"

      nonEmptyContexts [] = []
      nonEmptyContexts ctxs = [" (contexts: ", show ctxs, ")"]

instance Exception DurationParseError
