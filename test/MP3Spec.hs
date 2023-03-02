module MP3Spec where

import Test.Hspec

spec :: Spec
spec =
  describe "test" $ do
    it "works" $
      2 + 2 `shouldBe` (4 :: Int)
