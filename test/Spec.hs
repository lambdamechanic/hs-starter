module Main (main) where

import Starter.Prelude

import qualified Starter.Tests.Db as Db
import qualified Starter.Tests.Property as Property
import qualified Starter.Tests.Roboservant as Roboservant
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup
    "Starter"
    [ testCase "bootstrap sanity" (True @?= True)
    , Roboservant.tests
    , Property.tests
    , Db.tests
    ]
