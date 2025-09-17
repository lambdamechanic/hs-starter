module Starter.Tests.Db
  ( tests
  ) where

import Starter.Prelude

import Control.Exception (SomeException, try, displayException)
import qualified Database.Postgres.Temp as Temp
import qualified Squeal.PostgreSQL as PQ
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)

-- | Ensure tmp-postgres instances boot and accept Squeal connections.
tests :: TestTree
tests =
  testGroup
    "tmp-postgres"
    [ testCase "connects" tmpPostgresConnects
    ]

tmpPostgresConnects :: IO ()
tmpPostgresConnects = do
  startResult <- try @SomeException Temp.start
  case startResult of
    Left exc -> assertFailure ("tmp-postgres failed to start: " <> displayException exc)
    Right (Left err) -> assertFailure ("tmp-postgres unavailable: " <> show err)
    Right (Right db) -> do
      let connString = Temp.toConnectionString db
      PQ.withConnection connString (pure ())
      Temp.stop db
