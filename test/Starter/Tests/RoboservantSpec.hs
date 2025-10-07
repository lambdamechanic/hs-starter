{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Starter.Tests.RoboservantSpec (spec) where

import Control.Exception (SomeException, bracket, displayException, throwIO, try)
import Data.ByteString.Char8 qualified as B8
import qualified Data.ByteString.Char8 as BC
import Data.List (dropWhileEnd)
import Data.Text qualified as Text
import Database.Postgres.Temp qualified as Temp
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Types (Only (..))
import Minithesis qualified as Minithesis
import Minithesis.Property qualified as MP
import Roboservant.Server qualified as Robo
import Roboservant.Types (Atom (..), Breakdown)
import Roboservant.Types.Config (TraceCheck (..), defaultConfig)
import qualified Roboservant.Types.Config as RoboConfig
import Starter.Auth.Firebase (firebaseAuthDisabled)
import Starter.Auth.Session (SessionConfig (..))
import Starter.Database.Connection (DbConfig (..))
import Starter.Env (AppEnv (..))
import Starter.Prelude
import Starter.Server (HealthApi, HealthStatus, healthServer)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Syd
import Test.Syd.HList qualified as HList

spec :: Spec
spec =
  describe "Roboservant" $
    aroundAll withHealthAppEnv $ do
      itWithAll "covers healthcheck" $ \(envs :: HList.HList '[AppEnv]) () ->
        case envs of
          (HList.HCons env HList.HNil) -> runProperty (roboservantHealthProperty (env :: AppEnv))

-- | Wrapper for running Minithesis properties with project defaults.
runProperty :: MP.Property -> IO ()
runProperty property = do
  let base = MP.applyPropertyOptions Minithesis.defaultRunOptions property
  opts <- Minithesis.resolveRunOptions base
  MP.runProperty opts property

-- | Build the Roboservant property for the health check API.
roboservantHealthProperty :: AppEnv -> MP.Property
roboservantHealthProperty env =
  let traceConfig =
        defaultConfig
          { RoboConfig.traceChecks = [TraceCheck "noop" (const Nothing)]
          }
   in MP.withTests 1 (Robo.fuzzProperty @HealthApi (healthServer env) traceConfig)

-- | Prepare the application environment backed by a temporary Postgres.
withHealthAppEnv :: (AppEnv -> IO ()) -> IO ()
withHealthAppEnv use =
  bracket acquire release (\(_, env) -> use env)
  where
    acquire = do
      startResult <- try @SomeException Temp.start
      case startResult of
        Left exc -> do
          _ <- expectationFailure ("tmp-postgres failed to start: " <> displayException exc)
          throwIO exc
        Right (Left err) -> do
          _ <- expectationFailure ("tmp-postgres unavailable: " <> show err)
          throwIO (userError "tmp-postgres unavailable")
        Right (Right db) -> do
          env <- buildEnv db
          pure (db, env)

    release (db, _) = void (Temp.stop db)

    buildEnv db = do
      let connStr = B8.unpack (Temp.toConnectionString db)
          kvs = map (break (== '=')) (words connStr)
          lookupKV k = fmap (drop 1) (lookup k kvs)
      actualUser <- determineUser connStr
      let dbCfg =
            DbConfig
              { dbHost = maybe "localhost" Text.pack (lookupKV "host"),
                dbPort = maybe 5432 read (lookupKV "port"),
                dbName = maybe "postgres" Text.pack (lookupKV "dbname"),
                dbUser = maybe actualUser (Text.pack . stripQuotes) (lookupKV "user"),
                dbPassword = case lookupKV "password" of
                  Nothing -> Nothing
                  Just "" -> Nothing
                  Just p -> Just (Text.pack p)
              }
      applyPgrollMigrations dbCfg
      let env =
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
      pure env

-- Session configuration used for tests.
testSessionConfig :: SessionConfig
testSessionConfig =
  SessionConfig
    { sessionSecret = BC.pack "test-session-secret",
      sessionCookieName = BC.pack "hs_test_session",
      sessionCookieMaxAge = 3600,
      sessionLoginPath = "/login"
    }

-- | Find the Postgres user backing tmp-postgres.
determineUser :: String -> IO Text.Text
determineUser connStr =
  bracket (PG.connectPostgreSQL (B8.pack connStr)) PG.close $ \conn -> do
    rows <- PG.query_ conn "SELECT current_user"
    case rows of
      [Only userName] -> pure userName
      _ -> expectationFailure "failed to determine current_user" >> pure (Text.pack "postgres")

stripQuotes :: String -> String
stripQuotes = dropWhileEnd (== '\'') . dropWhile (== '\'')

-- | Apply the pgroll migrations in db/pgroll to the given connection.
applyPgrollMigrations :: DbConfig -> IO ()
applyPgrollMigrations cfg = do
  let DbConfig {dbHost, dbPort, dbName, dbUser, dbPassword} = cfg
      migrationsDir = "db/pgroll"
      hostStr = Text.unpack dbHost
      userStr = Text.unpack dbUser
      dbStr = Text.unpack dbName
      passStr = maybe "" (\p -> ":" <> Text.unpack p) dbPassword
      url =
        case hostStr of
          '/':_ ->
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
            let (pre, rest) = break (== '@') url
             in case break (== ':') pre of
                  (u, _) -> u <> ":***" <> rest
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

indentBlock :: String -> String
indentBlock = unlines . fmap ("  " <>) . lines

deriving via (Atom HealthStatus) instance Breakdown HealthStatus
