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
import Starter.Database.Connection (DbConfig (..))
import Starter.Auth.Firebase (firebaseAuthDisabled)
import Starter.Env (AppEnv (..))
import Starter.Prelude
import Starter.Server (HealthApi, HealthStatus, healthServer)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

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
                authorizeLogin = const (pure True),
                firebaseAuth = firebaseAuthDisabled
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
      -- Apply pgroll migrations against the tmp-postgres instance.
      applyPgrollMigrations dbCfg
      result <- Robo.fuzz @HealthApi (healthServer env) defaultConfig
      case result of
        Nothing -> putStrLn "roboservant: no counterexamples found"
        Just _ -> putStrLn "roboservant: counterexample found"
      Temp.stop db
      assertBool "roboservant found a counterexample" (isNothing result)

-- | Apply the pgroll migrations in db/pgroll to the given connection.
applyPgrollMigrations :: DbConfig -> IO ()
applyPgrollMigrations cfg = do
  let DbConfig {dbHost, dbPort, dbName, dbUser, dbPassword} = cfg
  let migrationsDir = "db/pgroll"
      hostStr = Text.unpack dbHost
      userStr = Text.unpack dbUser
      dbStr = Text.unpack dbName
      passStr = maybe "" (\p -> ":" <> Text.unpack p) dbPassword
      url =
        case hostStr of
          '/':_ ->
            -- Use unix socket via query params
            "postgres://"
              <> userStr
              <> passStr
              <> "/" -- empty authority, db after single slash
              <> dbStr
              <> "?host="
              <> hostStr
              <> "&port="
              <> show dbPort
              <> "&sslmode=disable"
          _ ->
            "postgres://"
              <> userStr
              <> passStr
              <> "@"
              <> hostStr
              <> ":"
              <> show dbPort
              <> "/"
              <> dbStr
              <> "?sslmode=disable"
      safeUrl =
        case dbPassword of
          Nothing -> url
          Just _ ->
            -- crude mask: replace ":<pass>@" with ":***@"
            let (pre, rest) = break (=='@') url in
            case break (==':') pre of
              (u, _colonAndPass) -> u <> ":***" <> rest
  putStrLn ("pgroll: init public schema and state schema pgroll")
  putStrLn ("pgroll: using URL " <> safeUrl)
  -- Ensure pgroll exists and log version
  (codeVer, outVer, errVer) <- readProcessWithExitCode "pgroll" ["--version"] ""
  putStrLn ("pgroll --version exit=" <> show codeVer <> "; stdout=\n" <> outVer <> "stderr=\n" <> errVer)
  (codeInit, outInit, errInit) <- readProcessWithExitCode "env" ["PGCONNECT_TIMEOUT=5","pgroll","init", "--postgres-url", url, "--schema", "public", "--pgroll-schema", "pgroll"] ""
  putStrLn ("pgroll init exit=" <> show codeInit)
  case codeInit of
    ExitSuccess -> putStrLn "pgroll: init ok"
    _ -> assertFailure ("pgroll init failed:\nSTDOUT:\n" <> outInit <> "\nSTDERR:\n" <> errInit)
  putStrLn ("pgroll: migrating from " <> migrationsDir)
  (codeMig, outMig, errMig) <- readProcessWithExitCode "env" ["PGCONNECT_TIMEOUT=5","pgroll","migrate", migrationsDir, "--postgres-url", url, "--schema", "public", "--pgroll-schema", "pgroll", "--complete"] ""
  putStrLn ("pgroll migrate exit=" <> show codeMig)
  case codeMig of
    ExitSuccess -> putStrLn "pgroll: migrations applied successfully"
    _ -> assertFailure ("pgroll migrate failed:\nSTDOUT:\n" <> outMig <> "\nSTDERR:\n" <> errMig)

tests :: TestTree
tests =
  testGroup
    "Roboservant"
    [testCase "covers healthcheck" fuzzesHealthcheck]

deriving via (Atom HealthStatus) instance Breakdown HealthStatus
