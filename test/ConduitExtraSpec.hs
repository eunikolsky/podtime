module ConduitExtraSpec (spec) where

import Conduit
import ConduitExtra
import Data.Conduit.List
import Data.Word
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "tailC" $ do
    prop "returns empty list for empty input" $ \n ->
      runConduitPure (sourceNull .| tailC n) `shouldBe` ([] :: [Int])

    prop "returns all elements in order for stream with exactly N elements"
      . forAll genSourceList $ \(n, source) ->
        runConduitPure (sourceList source .| tailC n) `shouldBe` source

-- | Generates a non-empty list of arbitrary length and contents.
genSourceList :: Gen (Word8, [Int])
genSourceList = do
  n <- getPositive <$> arbitrary
  list <- vectorOf (fromIntegral n) arbitrary
  pure (n, list)
