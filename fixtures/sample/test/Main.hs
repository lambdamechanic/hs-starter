module Main where

import Prelude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testCase "ping" $ ping @?= "pong"

ping :: String
ping = "pong"
