module Starter.Tests.Property
  ( tests,
  )
where

import Starter.Prelude
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (Property, gen, testFailed, testProperty)

-- | Property: addition is commutative for Int32.
tests :: TestTree
tests =
  testGroup
    "Property"
    [ testProperty "addition commutative" additionCommutative
    ]

additionCommutative :: Property ()
additionCommutative = do
  x <- gen (Gen.inRange (Range.withOrigin (-10_000, 10_000) (0 :: Int32)))
  y <- gen (Gen.inRange (Range.withOrigin (-10_000, 10_000) (0 :: Int32)))
  unless (x + y == y + x) $ do
    let detail = "addition failed for: " <> show (x, y)
    testFailed detail
