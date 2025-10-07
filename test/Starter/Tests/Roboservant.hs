{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Starter.Tests.Roboservant
  ( spec,
  )
where

import Control.Exception (SomeException, bracket, displayException, finally, try)
import Data.ByteString.Char8 qualified as B8
import Data.List (dropWhileEnd)
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import Database.Postgres.Temp qualified as Temp
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Types (Only (..))
import Roboservant.Server qualified as Robo
import Roboservant.Types (Atom (..), Breakdown)
import Roboservant.Types.Config (defaultConfig)
import Starter.Database.Connection (DbConfig (..))
import Starter.Auth.Firebase (firebaseAuthDisabled)
import Starter.Auth.Session (SessionConfig (..))
import qualified Data.ByteString.Char8 as BC
import Starter.Env (AppEnv (..))
import Starter.Prelude
import Starter.Server (HealthApi, HealthStatus, healthServer)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Test.Syd

testSessionConfig :: SessionConfig
testSessionConfig =
  SessionConfig
    { sessionSecret = BC.pack "test-session-secret",
      sessionCookieName = BC.pack "hs_test_session",
      sessionCookieMaxAge = 3600,
      sessionLoginPath = "/login"
    }

-- | Run the Roboservant fuzzer against the health check API.
fuzzesHealthcheck :: IO ()
fuzzesHealthcheck = do
  -- Boot a temporary Postgres for exercising the health endpoint
  startResult <- try @SomeException Temp.start
  case startResult of
    Left exc -> expectationFailure ("tmp-postgres failed to start: " <> displayException exc)
    Right (Left err) -> expectationFailure ("tmp-postgres unavailable: " <> show err)
    Right (Right db) ->
      let cleanup = void (Temp.stop db)
       in finally (runAgainstDb db) cleanup
  where
    runAgainstDb db = do
      let connStr = B8.unpack (Temp.toConnectionString db)
          kvs = map (break (== '=')) (words connStr)
          lookupKV :: String -> Maybe String
          lookupKV k = fmap (drop 1) (lookup k kvs)
      actualUser <- determineUser connStr
      let dbCfg =
            DbConfig
              { dbHost = maybe "localhost" Text.pack (lookupKV "host"),
                dbPort = maybe 5432 read (lookupKV "port"),
                dbName = maybe "postgres" Text.pack (lookupKV "dbname"),
                dbUser = maybe actualUser (Text.pack . stripQuotes) (lookupKV "user"),
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
                firebaseAuth = firebaseAuthDisabled,
                sessionConfig = testSessionConfig,
                frontendDir = "/opt/app/frontend"
              }
      let connectionCtx =
            unlines
              [ "tmp-postgres connection",
                "  host=" <> Text.unpack (dbHost dbCfg),
                "  port=" <> show (dbPort dbCfg),
                "  database=" <> Text.unpack (dbName dbCfg),
                "  user=" <> Text.unpack (dbUser dbCfg)
              ]
      -- Apply pgroll migrations against the tmp-postgres instance.
      context connectionCtx $ do
        applyPgrollMigrations dbCfg
        result <- Robo.fuzz @HealthApi (healthServer env) defaultConfig
        let resultCtx =
              case result of
                Nothing -> "roboservant: no counterexample"
                Just counterexample ->
                  "roboservant counterexample:\n" <> indentBlock (show counterexample)
        context resultCtx $ result `shouldSatisfy` isNothing

    determineUser :: String -> IO Text
    determineUser connStr =
      bracket (PG.connectPostgreSQL (B8.pack connStr)) PG.close $ \conn -> do
        rows <- PG.query_ conn "SELECT current_user"
        case rows of
          [Only userName] -> pure userName
          _ -> expectationFailure "failed to determine current_user" >> pure (Text.pack "postgres")

stripQuotes :: String -> String
stripQuotes = dropWhileEnd (== '\'') . dropWhile (== '\'')

indentBlock :: String -> String
indentBlock = unlines . fmap ("  " <>) . lines

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
            -- Use unix socket via query params; authority left empty with '@/'
            "postgres://"
              <> userStr
              <> passStr
              <> "@/"
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
  let pgrollContext label exitCode stdoutText stderrText =
        unlines
          [ label <> " exit=" <> show exitCode,
            "stdout:",
            indentBlock stdoutText,
            "stderr:",
            indentBlock stderrText
          ]
  context ("pgroll init public schema; url=" <> safeUrl) $ do
    (codeVer, outVer, errVer) <- readProcessWithExitCode "pgroll" ["--version"] ""
    context (pgrollContext "pgroll --version" codeVer outVer errVer) $
      codeVer `shouldBe` ExitSuccess
    (codeInit, outInit, errInit) <-
      readProcessWithExitCode
        "env"
        [ "PGCONNECT_TIMEOUT=5",
          "pgroll",
          "init",
          "--postgres-url",
          url,
          "--schema",
          "public",
          "--pgroll-schema",
          "pgroll"
        ]
        ""
    context (pgrollContext "pgroll init" codeInit outInit errInit) $
      codeInit `shouldBe` ExitSuccess
    (codeMig, outMig, errMig) <-
      readProcessWithExitCode
        "env"
        [ "PGCONNECT_TIMEOUT=5",
          "pgroll",
          "migrate",
          migrationsDir,
          "--postgres-url",
          url,
          "--schema",
          "public",
          "--pgroll-schema",
          "pgroll",
          "--complete"
        ]
        ""
    context (pgrollContext "pgroll migrate" codeMig outMig errMig) $
      codeMig `shouldBe` ExitSuccess

spec :: Spec
spec =
  describe "Roboservant" $ do
    it "covers healthcheck" fuzzesHealthcheck

deriving via (Atom HealthStatus) instance Breakdown HealthStatus
