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

    prop "returns all elements in order from stream with exactly N elements"
      . forAll genSourceList $ \(n, source) ->
        runConduitPure (sourceList source .| tailC n) `shouldBe` source

    prop "returns all elements from stream with fewer elements"
      . forAll genSourceListAtLeastTwoElements $ \(sourceCount, source) -> do
        let n = sourceCount + 1
        runConduitPure (sourceList source .| tailC n) `shouldBe` source

-- | Generates a non-empty list of arbitrary length and contents.
genSourceList :: Gen (Word8, [Int])
genSourceList = do
  n <- getPositive <$> arbitrary
  list <- vectorOf (fromIntegral n) arbitrary
  pure (n, list)

-- | Generates an arbitrary list of 2+ elements.
genSourceListAtLeastTwoElements :: Gen (Word8, [Int])
genSourceListAtLeastTwoElements = do
  n <- chooseEnum (2, maxBound - 1)
  list <- vectorOf (fromIntegral n) arbitrary
  pure (n, list)
