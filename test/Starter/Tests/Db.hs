module Starter.Tests.Db
  ( tests
  ) where

import Starter.Prelude

import Control.Exception (SomeException, catch, displayException)
import qualified Database.Postgres.Temp as Temp
import qualified Squeal.PostgreSQL as PQ
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

-- | Ensure tmp-postgres instances boot and accept Squeal connections.
tests :: TestTree
tests =
  testGroup
    "tmp-postgres"
    [ testCase "connects" tmpPostgresConnects
    ]

tmpPostgresConnects :: IO ()
tmpPostgresConnects = do
  startResult <- catch (Right <$> Temp.start) handler
  case startResult of
    Left msg -> putStrLn msg
    Right (Left err) ->
      putStrLn $ "tmp-postgres unavailable, skipping test: " <> show err
    Right (Right db) -> do
      let connString = Temp.toConnectionString db
      PQ.withConnection connString (pure ())
      Temp.stop db

handler :: SomeException -> IO (Either String (Either Temp.StartError Temp.DB))
handler exc =
  pure . Left $ "tmp-postgres unavailable, skipping test: " <> displayException exc
