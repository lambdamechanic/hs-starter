{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Starter.Tests.Roboservant
  ( tests,
  )
where

import Control.Exception (SomeException, catch, displayException, try)
import Data.ByteString.Char8 qualified as B8
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import Database.Postgres.Temp qualified as Temp
import Roboservant.Server qualified as Robo
import Roboservant.Types (Atom (..), Breakdown)
import Roboservant.Types.Config (defaultConfig)
import Squeal.PostgreSQL
import Squeal.PostgreSQL.Definition.Constraint (primaryKey, unique)
import Squeal.PostgreSQL.Definition.Table (createTableIfNotExists)
import Squeal.PostgreSQL.Expression.Time (now)
import Squeal.PostgreSQL.Expression.Type (bool, default_, notNullable, nullable, serial, text, timestamptz)
import Starter.Database.Connection (DbConfig (..), withAppConnection)
import Starter.Env (AppEnv (..))
import Starter.OAuth.Types (OAuthProfile)
import Starter.Prelude
import Starter.Server (HealthApi, HealthStatus, healthServer)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

-- | Run the Roboservant fuzzer against the health check API.
fuzzesHealthcheck :: IO ()
fuzzesHealthcheck = do
  -- Boot a temporary Postgres for exercising the health endpoint
  startResult <- try @SomeException Temp.start
  case startResult of
    Left exc -> assertFailure ("tmp-postgres failed to start: " <> displayException exc)
    Right (Left err) -> assertFailure ("tmp-postgres unavailable: " <> show err)
    Right (Right db) -> do
      let connStr = B8.unpack (Temp.toConnectionString db)
          kvs = map (break (== '=')) (words connStr)
          lookupKV :: String -> Maybe String
          lookupKV k = fmap (drop 1) (lookup k kvs)
          dbCfg =
            DbConfig
              { dbHost = maybe "localhost" Text.pack (lookupKV "host"),
                dbPort = maybe 5432 read (lookupKV "port"),
                dbName = maybe "postgres" Text.pack (lookupKV "dbname"),
                dbUser = maybe "postgres" Text.pack (lookupKV "user"),
                dbPassword =
                  ( case lookupKV "password" of
                      Nothing -> Nothing
                      Just "" -> Nothing
                      Just p -> Just (Text.pack p)
                  )
              }
          env =
            AppEnv
              { appPort = 0,
                otelServiceName = "hs-starter-tests",
                otelCollectorEndpoint = Nothing,
                otelCollectorHeaders = Nothing,
                dbConfig = dbCfg,
                authorizeLogin = const (pure True)
              }
      putStrLn
        ( "tmp-postgres connection: host="
            <> Text.unpack (dbHost dbCfg)
            <> " port="
            <> show (dbPort dbCfg)
            <> " dbname="
            <> Text.unpack (dbName dbCfg)
            <> " user="
            <> Text.unpack (dbUser dbCfg)
        )
      -- Ensure minimal schema required for health check exists.
      let createUsers =
            createTableIfNotExists
              (#public ! #users)
              ( serial
                  `as` #id
                  :* (text & nullable)
                  `as` #email
                  :* (text & nullable)
                  `as` #display_name
                  :* (text & nullable)
                  `as` #avatar_url
                  :* (timestamptz & notNullable & default_ now)
                  `as` #created_at
                  :* (timestamptz & notNullable & default_ now)
                  `as` #updated_at
                  :* (text & notNullable)
                  `as` #provider
                  :* (text & notNullable)
                  `as` #subject
                  :* (bool & notNullable)
                  `as` #allowed
                  :* (timestamptz & nullable)
                  `as` #last_login_at
              )
              ( primaryKey #id
                  `as` #users_pkey
                  :* unique (#provider :* #subject :* Nil)
                  `as` #users_provider_subject_key
                  :* Nil
              )
      ddlResult <- try @SomeException (withAppConnection dbCfg (define createUsers))
      case ddlResult of
        Left exc -> do
          putStrLn ("failed to ensure users table: " <> displayException exc)
          assertFailure ("failed to define users table: " <> displayException exc)
        Right _ -> pure ()
      result <- Robo.fuzz @HealthApi (healthServer env) defaultConfig
      case result of
        Nothing -> putStrLn "roboservant: no counterexamples found"
        Just _ -> putStrLn "roboservant: counterexample found"
      Temp.stop db
      assertBool "roboservant found a counterexample" (isNothing result)

tests :: TestTree
tests =
  testGroup
    "Roboservant"
    [testCase "covers healthcheck" fuzzesHealthcheck]

deriving via (Atom HealthStatus) instance Breakdown HealthStatus
