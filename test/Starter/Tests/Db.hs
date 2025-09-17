module Starter.Tests.Db
  ( tests,
  )
where

import Control.Exception (SomeException, displayException, try)
import Database.Postgres.Temp qualified as Temp
import Squeal.PostgreSQL qualified as PQ
import Starter.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

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
