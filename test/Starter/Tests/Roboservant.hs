{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Starter.Tests.Roboservant
  ( tests
  ) where

import Starter.Prelude

import Data.Maybe (isNothing)
import qualified Roboservant.Server as Robo
import Roboservant.Types (Atom (..), Breakdown)
import Roboservant.Types.Config (defaultConfig)
import Starter.Server (HealthApi, HealthStatus, healthServer)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import qualified Database.Postgres.Temp as Temp
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as Text
import Control.Exception (SomeException, catch)
import Starter.Env (AppEnv (..))
import Starter.Database.Connection (DbConfig (..), withAppConnection)
import Starter.OAuth.Types (OAuthProfile)
import Squeal.PostgreSQL
import Squeal.PostgreSQL.Definition.Table (createTableIfNotExists)
import Squeal.PostgreSQL.Definition.Constraint (primaryKey, unique)
import Squeal.PostgreSQL.Expression.Type (text, bool, timestamptz, nullable, notNullable, default_, serial)
import Squeal.PostgreSQL.Expression.Time (now)

-- | Run the Roboservant fuzzer against the health check API.
fuzzesHealthcheck :: IO ()
fuzzesHealthcheck = do
  -- Boot a temporary Postgres for exercising the health endpoint
  startResult <- catch (Right <$> Temp.start) (\(_ :: SomeException) -> pure (Left "exc"))
  case startResult of
    Left _ -> pure ()
    Right (Left _err) ->
      -- tmp-postgres reported unavailable; skip test
      pure ()
    Right (Right db) -> do
      let connStr = B8.unpack (Temp.toConnectionString db)
          kvs = map (break (=='=')) (words connStr)
          lookupKV :: String -> Maybe String
          lookupKV k = fmap (drop 1) (lookup k kvs)
          dbCfg = DbConfig
            { dbHost = maybe "localhost" Text.pack (lookupKV "host")
            , dbPort = maybe 5432 read (lookupKV "port")
            , dbName = maybe "postgres" Text.pack (lookupKV "dbname")
            , dbUser = maybe "postgres" Text.pack (lookupKV "user")
            , dbPassword = (
                case lookupKV "password" of
                  Nothing -> Nothing
                  Just "" -> Nothing
                  Just p -> Just (Text.pack p)
              )
            }
          env = AppEnv
            { appPort = 0
            , otelServiceName = "hs-starter-tests"
            , otelCollectorEndpoint = Nothing
            , otelCollectorHeaders = Nothing
            , dbConfig = dbCfg
            , authorizeLogin = const (pure True)
            }
      -- Ensure minimal schema required for health check exists.
      let createUsers =
            createTableIfNotExists (#public ! #users)
              ( serial `as` #id
                  :* (text & nullable) `as` #email
                  :* (text & nullable) `as` #display_name
                  :* (text & nullable) `as` #avatar_url
                  :* (timestamptz & notNullable & default_ now) `as` #created_at
                  :* (timestamptz & notNullable & default_ now) `as` #updated_at
                  :* (text & notNullable) `as` #provider
                  :* (text & notNullable) `as` #subject
                  :* (bool & notNullable) `as` #allowed
                  :* (timestamptz & nullable) `as` #last_login_at
              )
              ( primaryKey #id `as` #users_pkey
                  :* unique (#provider :* #subject :* Nil) `as` #users_provider_subject_key
                  :* Nil
              )
      _ <- withAppConnection dbCfg (define createUsers)
      result <- Robo.fuzz @HealthApi (healthServer env) defaultConfig
      Temp.stop db
      assertBool "roboservant found a counterexample" (isNothing result)

tests :: TestTree
tests =
  testGroup
    "Roboservant"
    [testCase "covers healthcheck" fuzzesHealthcheck]

deriving via (Atom HealthStatus) instance Breakdown HealthStatus
