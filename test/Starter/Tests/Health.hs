{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Starter.Tests.Health
  ( tests,
  )
where

import Control.Exception (SomeException, assert, displayException, try)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as Text
import Database.Postgres.Temp qualified as Temp
import Network.HTTP.Types (methodGet, status200)
import Network.Wai (Application, defaultRequest, rawPathInfo, requestMethod)
import Network.Wai.Test
import Squeal.PostgreSQL
import Squeal.PostgreSQL.Definition.Constraint (primaryKey, unique)
import Squeal.PostgreSQL.Definition.Table (createTableIfNotExists)
import Squeal.PostgreSQL.Expression.Time (now)
import Squeal.PostgreSQL.Expression.Type (bool, default_, notNullable, nullable, serial, text, timestamptz)
import Starter.Database.Connection (DbConfig (..), withAppConnection)
import Starter.Env (AppEnv (..))
import Starter.Prelude
import Starter.Server (HealthStatus (..), app)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Health"
    [ testCase "health returns 200 after DB init" healthOk
    ]

healthOk :: IO ()
healthOk = do
  -- Start an ephemeral Postgres
  eDb <- try @SomeException Temp.start
  case eDb of
    Left exc -> assertFailure ("tmp-postgres failed to start: " <> displayException exc)
    Right (Left err) -> assertFailure ("tmp-postgres unavailable: " <> show err)
    Right (Right db) -> do
      let kvs = map (break (== '=')) (words (BS.unpack (Temp.toConnectionString db)))
          lookupKV k = fmap (drop 1) (lookup k kvs)
          dbCfg =
            DbConfig
              { dbHost = maybe "localhost" Text.pack (lookupKV "host"),
                dbPort = maybe 5432 read (lookupKV "port"),
                dbName = maybe "postgres" Text.pack (lookupKV "dbname"),
                dbUser = maybe "postgres" Text.pack (lookupKV "user"),
                dbPassword = case lookupKV "password" of
                  Nothing -> Nothing
                  Just "" -> Nothing
                  Just p -> Just (Text.pack p)
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
      -- Ensure the minimal schema exists for /health (users table)
      let createUsers =
            createTableIfNotExists
              (#public ! #users)
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

      -- Exercise the WAI application directly
      let application :: Application
          application = app env
      res <- runSession (srequest (SRequest defaultRequest {requestMethod = methodGet, rawPathInfo = "/health"} "")) application
      let code = simpleStatus res
      when (code /= status200) $ do
        assertFailure ("/health did not return 200, got: " <> show code <> "; body=" <> show (simpleBody res))
      Temp.stop db
