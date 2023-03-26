module ConduitExtraSpec (spec) where

import Conduit
import ConduitExtra
import Data.Conduit.List
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "tailC" $ do
    prop "returns empty list for empty input" $ \n ->
      runConduitPure (sourceNull .| tailC n) `shouldBe` ([] :: [Int])
