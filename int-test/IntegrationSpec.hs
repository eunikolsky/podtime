module IntegrationSpec (main) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "mp3Parser" .
    it "parses actual podcast episodes" $ do
      pending
