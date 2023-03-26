module ConduitExtraSpec (spec) where

import Conduit
import ConduitExtra
import Data.Conduit.List (sourceList, sourceNull)
import Data.Word
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TestCommon

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

    prop "returns N last elements from stream with more elements"
      . forAll genSourceListWithMoreElements $ \(source, n, expected) ->
        runConduitPure (sourceList source .| tailC n) `shouldBe` expected

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

-- | Generates an arbitrary list of 2+ elements, `n` less than the list length,
-- and the last `n` elements of the list.
genSourceListWithMoreElements :: Gen ([Int], Word8, [Int])
genSourceListWithMoreElements = do
  list <- (:) <$> arbitrary <*> listOf1 arbitrary
  n <- chooseEnum (1, fromIntegral $ length list `noMoreThan` fromIntegral (maxBound :: Word8))
  let lastN = takeLast n list
  pure (list, n, lastN)

takeLast :: Word8 -> [a] -> [a]
takeLast n = reverse . take (fromIntegral n) . reverse
