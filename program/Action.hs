module Action
  ( Action(..)
  , actionParser
  ) where

import Control.Applicative (asum)
import Options.Applicative (Parser, flag', help, long, metavar, short, strArgument)

-- | Describes an action requested by the user.
data Action
  = PrintAllDurations
  | PrintFileDuration FilePath
  | PrintVersion

actionParser :: Parser Action
actionParser = asum [printVersion, printFileDuration, printAllDurations]

printFileDuration :: Parser Action
printFileDuration = PrintFileDuration <$> strArgument
  ( metavar "FILE"
  <> help "Print the duration of a single FILE (w/o using cache)"
  )

printVersion :: Parser Action
printVersion = flag' PrintVersion
  ( long "version"
  <> short 'v'
  <> help "Show program version"
  )

-- | `PrintAllDurations` is the default action if no options are provided, so
-- it always succeeds and must be used as the last alternative.
printAllDurations :: Parser Action
printAllDurations = pure PrintAllDurations
