module Main (main) where

import Starter.Prelude
import Starter.Tests.Db qualified as Db
import Starter.Tests.Property qualified as Property
import Starter.Tests.Roboservant qualified as Roboservant
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup
    "Starter"
    [ testCase "bootstrap sanity" (True @?= True),
      Roboservant.tests,
      Property.tests,
      Db.tests
    ]
