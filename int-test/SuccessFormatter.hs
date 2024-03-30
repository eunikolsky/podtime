module SuccessFormatter
  ( successFormatter
  ) where

import Test.Hspec.Api.Formatters.V1
import Text.Printf

-- TODO this uses v1 formatters, which is deprecated
successFormatter :: Formatter
successFormatter = checks { footerFormatter = footerFormatter checks >> printSuccessRate }

-- | Prints the number of successful test cases and their percentage.
printSuccessRate :: FormatM ()
printSuccessRate = do
  successCount <- getSuccessCount
  totalCount <- getTotalCount
  let successPercentage = (fromIntegral @_ @Float successCount / fromIntegral totalCount) * 100

  withSuccessColor . writeLine $
    printf "Successful examples: %d/%d (%0.2f%%)" successCount totalCount successPercentage
